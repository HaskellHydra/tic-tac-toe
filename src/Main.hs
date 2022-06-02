{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE  RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import qualified Data.Aeson as Aeson
import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Default               (def, Default (..))
import           Data.Text            (pack, Text)
import Ledger (Address, dataHash ,Datum (Datum), DatumHash (..), ScriptContext, Validator, Value, getCardanoTxId)
import Ledger 
import Ledger.Value as Value
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Typed.Scripts qualified as Scripts
import Playground.Contract
import Plutus.Contract
import qualified PlutusTx         as PlutusTx
import PlutusTx.Prelude hiding (pure, (<$>))
import Prelude qualified as Haskell
import           Text.Printf          (printf)
import Ledger.TimeSlot

-- Import Plutus Map
import PlutusTx.AssocMap qualified as AssocMap

-- Trace imports
import qualified Plutus.Trace as Trace
import Wallet.Emulator.Wallet
import qualified Control.Monad.Freer.Extras as Extras
import Wallet.Emulator.MultiAgent
import Plutus.Trace.Emulator.Types
import System.IO

-- Builtins
import           PlutusTx.Builtins.Internal (BuiltinString(..), BuiltinByteString (..), decodeUtf8)

type AMap = AssocMap.Map 

minLovelace :: Integer
minLovelace = 2_000_000

minGameStake :: Integer
minGameStake = 10_000_000

data GameChoice = X | O | N
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    X == X = True
    O == O = True
    N == N = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''GameChoice

data Game = Game
    { tFirstPlayer        :: !(BuiltinByteString, PaymentPubKeyHash)
    , tSecondPlayer       :: !(Maybe (BuiltinByteString, PaymentPubKeyHash))
    , tGameStake          :: !Integer
    , tMinGameStake       :: !Integer
    , tDeadline           :: !POSIXTime
    , identifierToken     :: !AssetClass
    , nextMove            :: !GameChoice -- keep track of the next move
    } deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq, Haskell.Ord)

instance Eq Game where
    {-# INLINABLE (==) #-}
    a == b =
        (tFirstPlayer a == tFirstPlayer b) &&
        (tSecondPlayer a == tSecondPlayer b) &&
        (tGameStake a == tGameStake b) &&
        (tDeadline a == tDeadline b) &&
        (identifierToken a == identifierToken b)

PlutusTx.unstableMakeIsData ''Game

data GameState = GameState (AMap BuiltinByteString GameChoice)
    deriving (Show, Generic, ToJSON, FromJSON, Haskell.Eq)

instance Eq GameState where
    {-# INLINABLE (==) #-}
    GameState cs == GameState cs' = (cs == cs')

PlutusTx.unstableMakeIsData ''GameState

data GameDatum = GameDatum {
    gdGame :: !Game
   ,gdGameState :: !GameState    
  }
  deriving (Show)

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | ClaimFirst | ClaimSecond
    deriving (Show, Generic, ToJSON, FromJSON, Haskell.Eq)

instance Eq GameRedeemer where
    {-# INLINABLE (==) #-}
    Play X == Play X = True
    Play O == Play O = True
    ClaimFirst == ClaimFirst = True
    ClaimSecond == ClaimSecond = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE str2GameChoice #-}
str2GameChoice :: BuiltinByteString -> Maybe GameChoice
str2GameChoice x 
                | (x == (BuiltinByteString Haskell.. C.pack) "X") = Just X
                | (x == (BuiltinByteString Haskell.. C.pack) "O") = Just O
                | otherwise = Nothing

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE initGameState #-}
initGameState :: GameState
initGameState = GameState (AssocMap.fromList t)
              where 
                  y = (\x -> (("p" Haskell.<> Haskell.show x) <> ) Haskell.<$> ((Haskell.show) Haskell.<$> [1..3::Integer] )) Haskell.<$> [1..3::Integer]
                  t = zip ((toBuiltin . C.pack) Haskell.<$> (concat y)) (Haskell.take 9 $ Haskell.repeat N)

{-# INLINABLE getGameStateMap #-}
getGameStateMap :: GameState -> (AMap BuiltinByteString GameChoice)
getGameStateMap (GameState m) = m 


{-# INLINABLE mkValidator #-}
mkValidator :: GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkValidator datum redeemer ctx = (traceIfFalse "Wrong game marker selection by the player" checkPlayerChoice ) &&
                                 (traceIfFalse "Deadline expired! (or) Unable to extract datum" checkDeadline)                                 
    where
        checkDeadline :: Bool
        checkDeadline = contains (to $ tDeadline $ gdGame datum) (txInfoValidRange $ scriptContextTxInfo ctx)

        -- If the second player is nothing in the output datum and the if the input is 'Just x' it is false
        -- If the second player is nothing both in input and output datums then player 2 did not join the game
          -- The game stake is only 1* gamestake      

        -- evalGameTx :: Bool
        -- evalGameTx = let inGameCfg = gdGame GameDatum
        --                  outGameCfg = gdGame outputDatum in
        --                    case (datum , redeemer) of
                              
        inGameCfg :: Game
        inGameCfg = gdGame datum

        outGameCfg :: Game
        outGameCfg = gdGame outputDatum

        checkPlayerChoice :: Bool
        checkPlayerChoice = case redeemer of
                              Play c -> if (c == X) then                                          
                                          (txSignedBy info $ unPaymentPubKeyHash $ snd $ tFirstPlayer inGameCfg ) &&
                                          ( traceIfFalse "Player1 trying to tamper with the next move marker !" $ c /= (nextMove outGameCfg))
                                        else if ( c == O ) then
                                          case (tSecondPlayer inGameCfg) of
                                            Nothing -> False 
                                            Just x -> (txSignedBy info $ unPaymentPubKeyHash $ snd x ) &&
                                                      ( traceIfFalse "Player2 trying to tamper with the next move marker !" $ c /= (nextMove outGameCfg))
                                        else
                                            traceError "Wrong game choice"
                              ClaimFirst -> True  -- TODO
                              ClaimSecond -> True -- TODO  

    --    verifyNextMove :: Bool
    --    verifyNextMove = case redeemer of
    --                          Play c -> if (c == X) then                                          
    --                                      (stringToBuiltinByteString "X") == (nextMove inGameCfg)
    --                                    else if ( c == O ) then
    --                                      case (tSecondPlayer inGameCfg) of
    --                                        Nothing -> False 
    --                                        Just x -> (txSignedBy info $ unPaymentPubKeyHash $ snd x )
    --                                    else
    --                                        traceError "Wrong game choice"
    --                          ClaimFirst -> True  -- TODO
    --                          ClaimSecond -> True -- TODO  



        -- checkCorrectDatum = 
        --   let inGameCfg = gdGame GameDatum
        --       outGameCfg = gdGame outputDatum in
        --     case ( tSecondPlayer inGameCfg ) of
        --       Nothing -> if (( tSecondPlayer inGameCfg ) /= (tSecondPlayer outGameCfg)) then
        --                  (( lovelaces $ txOutValue ownInput) == gStake) && 
        --                  ((lovelaces $ txOutValue ownInput) >= minGameStake)
        --                  else
        --                    (( lovelaces $ txOutValue ownInput) == gStake) && 
        --                    (( lovelaces $ txOutValue ownOutput) == 2*gStake) &&
        --                    ((lovelaces $ txOutValue ownInput) >= minGameStake)
        --       Just x -> True 
                       
        info :: TxInfo    
        info = scriptContextTxInfo ctx

        ownInput :: TxOut
        ownInput = case (findOwnInput ctx) of
                    Nothing -> traceError "game input is missing"
                    Just x -> txInInfoResolved x

        -- checkMinStake :: Bool
        -- checkMinStake 

        -----------------------------------------
        -- TODO: In the below code check if 
        -- the `findDatum h info` is extracting 
        -- the datum from the output.
        -----------------------------------------

        ownOutput   :: TxOut
        outputDatum :: GameDatum
        (ownOutput, outputDatum) = case getContinuingOutputs ctx of
            [o] -> case txOutDatumHash o of
                Nothing   -> traceError "wrong output type"
                Just h -> case findDatum h info of
                    Nothing        -> traceError "datum not found"
                    Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                        Just ad' -> (o, ad')
                        Nothing  -> traceError "error decoding data"
            _   -> traceError "expected exactly one continuing output"

-- | Datum and redeemer parameter types
data TicTacToe
instance Scripts.ValidatorTypes TicTacToe where
    type instance RedeemerType TicTacToe = GameRedeemer
    type instance DatumType TicTacToe = GameDatum

ticTacToeInstance ::Scripts.TypedValidator TicTacToe
ticTacToeInstance = Scripts.mkTypedValidator @TicTacToe
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

------------------------------------------------------------
-- | Off-Chain code
------------------------------------------------------------

validator :: Validator
validator = Scripts.validatorScript ticTacToeInstance

-- | The address of the bounty (the hash of its validator script)
gameAddress :: Address
gameAddress = Ledger.scriptAddress (Scripts.validatorScript ticTacToeInstance)

gameHash :: Ledger.ValidatorHash
gameHash = Scripts.validatorHash ticTacToeInstance


-- Start Game Params
data StartParams = StartParams {
    gFirstPlayerPkh      :: !PaymentPubKeyHash
   ,gFirstPlayerMarker   :: !BuiltinByteString
   ,gSecondPlayerPkh     :: !(Maybe PaymentPubKeyHash)
   ,gSecondPlayerMarker  :: !(Maybe BuiltinByteString)
   ,gNextMove            :: !GameChoice
   ,gStake               :: !Integer
   ,gDeadline            :: !POSIXTime
   ,gCurrency            :: !CurrencySymbol
   ,gTokenName           :: !TokenName
   ,gState               :: !GameState
   ,gMinGameStake        :: !Integer 
} deriving (Generic, ToJSON, FromJSON)

-- Move params - Redeemer
data MoveParams = MoveParams {
    mFirstPlayerPkh      :: !PaymentPubKeyHash
   ,mFirstPlayerMarker   :: !BuiltinByteString
   ,mSecondPlayerPkh     :: !(Maybe PaymentPubKeyHash)
   ,mSecondPlayerMarker  :: !(Maybe BuiltinByteString)
   ,mNextMove            :: !GameChoice   
   ,mStake               :: !Integer
   ,mCurrency            :: !CurrencySymbol
   ,mTokenName           :: !TokenName
   ,mState               :: !GameState
   ,mMinGameStake        :: !Integer 
   ,mChoice              :: !GameRedeemer
} deriving (Generic, ToJSON, FromJSON)

-- | The schema of the contract, with one endpoint to publish the problem with a bounty and another to sbumit solutions
type TicTacToeSchema = Endpoint  "start" StartParams
                       .\/ Endpoint "move" MoveParams
                       .\/ Endpoint "test" ()

start :: AsContractError e => StartParams -> Contract w s e ()
start StartParams{..} = do
    logInfo @Haskell.String "---------------- Start Game -------------------------"  
    pkh <- ownPaymentPubKeyHash
    let g = Game
              {
                tFirstPlayer = (gFirstPlayerMarker, gFirstPlayerPkh)
               ,tSecondPlayer = Nothing
               ,tMinGameStake = gMinGameStake
               ,tGameStake = gStake
               ,tDeadline = gDeadline
               ,nextMove = gNextMove
               ,identifierToken = AssetClass (gCurrency, gTokenName)
              }       
        d = GameDatum
            {
                gdGame = g
               ,gdGameState = gState
            }
        -- v = (Value.singleton gCurrency gTokenName 1) <> (Ada.lovelaceValueOf minLovelace ) <> (Ada.lovelaceValueOf gStake )
        v = (Value.singleton gCurrency gTokenName 1) <> (Ada.lovelaceValueOf gStake )        
        -- v = (Value.assetClassValue (Value.AssetClass (gCurrency, gTokenName)) 1) <> Ada.lovelaceValueOf minLovelace
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints ticTacToeInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Haskell.String $ printf "Started game with paramters %s and token %s" (Haskell.show d) (Haskell.show v)


-- mkMove :: AsContractError e => MoveParams -> Contract w s e ()
mkMove :: MoveParams -> Contract w s Text ()
mkMove MoveParams{..} = do
    logInfo @Haskell.String "-------------- Make a Move ---------------"
    (oref, o, d@GameDatum{..}) <- findGameDatum mCurrency mTokenName
    logInfo @Haskell.String $ printf "Datum from findDatum %s" (Haskell.show $ d)
    logInfo @Haskell.String $ printf "Using the next move as : %s" (Haskell.show mNextMove)
    let modGameCfg = gdGame { nextMove = mNextMove}
        d' = d { gdGame = modGameCfg }
        r = Redeemer $ PlutusTx.toBuiltinData $ Play X
        v = (Value.singleton mCurrency mTokenName 1) <> (Ada.lovelaceValueOf mStake )
        -- explained here https://cardano.stackexchange.com/questions/2296/lecture-6-it-2-core-hs-explaining-lookups-use-of-both-typedvalidatorlook
        lookups = Constraints.typedValidatorLookups ticTacToeInstance Haskell.<> -- used for the output utxo with the new contract instance
                  Constraints.otherScript validator                   Haskell.<> -- used for consuming the input contract instance
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx = Constraints.mustPayToTheScript d' v                            <>
             Constraints.mustValidateIn (to $ tDeadline gdGame)          <>
             Constraints.mustSpendScriptOutput oref r
    logInfo @Haskell.String $ printf "Modified Datum : %s" (Haskell.show d')    
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx


findGameDatum :: CurrencySymbol -> TokenName -> Contract w s Text (TxOutRef, ChainIndexTxOut, GameDatum)
findGameDatum cs tn = do
    utxos <- utxosAt $ scriptHashAddress gameHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (_ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@GameDatum{..}
                    | AssetClass(cs, tn) == (identifierToken gdGame) -> return (oref, o, d)
                    | otherwise                                           -> throwError "game token missmatch"
        _           -> throwError "game utxo not found"

test :: forall w s e. AsContractError e => Contract w s e ()
test = do
    logInfo @Haskell.String "-----------Printing test logs---------"
    logInfo @Haskell.String  $ "Game Address : " <> (Haskell.show gameAddress)

    unspentOutputs <- utxosAt gameAddress
    -- 'collectFromScript' is a function of the wallet API. It creates a tx consuming
    -- all unspent transaction outputs at a script address and pays them to a
    -- public key address owned by this wallet.
    logInfo @Haskell.String $ Haskell.show $ Map.toList unspentOutputs

    -- utxos at the gameHash
    utxos <- utxosAt $ scriptHashAddress gameHash -- equivalent to "unspentOutputs <- utxosAt gameAddress"
    logInfo @Haskell.String $  "Utxos at Game Hash : " <> (Haskell.show $ Map.toList utxos)

-- | TicTacToe endpoints.
endpoints :: Contract () TicTacToeSchema Text ()
endpoints = awaitPromise (start' `select` move' `select` test') >> endpoints
  where
    start' = endpoint @"start" start
    move' = endpoint @"move" mkMove
    test' = endpoint @"test" $ const test

-- mkSchemaDefinitions ''TicTacToeSchema

emulatorConfig :: Trace.EmulatorConfig
emulatorConfig =
  Trace.EmulatorConfig
    (Left $ Map.fromList [(knownWallet 1, v1),
                          (knownWallet 2, v2)])
    def
    def
      where
        -- Initialize the wallet with 100Ada and the NFT token
        -- In real-world you need to mint a NFT and assign it to the player 1
        v1 :: Value
        v1 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 1 
        v2 :: Value
        v2 = Ada.lovelaceValueOf 100_000_000

customShowEvent :: EmulatorEvent' -> Maybe Haskell.String
customShowEvent = \case
  UserThreadEvent (UserLog msg)                                        -> Just $ "*** USER LOG: " <> msg
  InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _)   -> Just $ "*** CONTRACT LOG: " <> Haskell.show msg
  InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> Haskell.show err
  ev                                                                   -> Nothing

traceConfig :: Trace.TraceConfig
traceConfig =
  Trace.TraceConfig
    customShowEvent
    stdout

-- test consuming the bounty when the deadline has not been reached
myTraceTest :: Trace.EmulatorTrace ()
myTraceTest = do
    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints 
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints 
    Trace.callEndpoint @"test" h1 ()
    -- s <- Trace.waitNSlots 1
    void $ Trace.waitNSlots 1
    -- Second player has not joined the game yet.
    Trace.callEndpoint @"start" h1 $ StartParams
      {
        gFirstPlayerPkh = mockWalletPaymentPubKeyHash (knownWallet 1)
       ,gFirstPlayerMarker = (BuiltinByteString Haskell.. C.pack) "X"
       ,gSecondPlayerPkh = Nothing -- mockWalletPaymentPubKeyHash (knownWallet 2)
       ,gSecondPlayerMarker = Nothing -- (BuiltinByteString Haskell.. C.pack) "O"
       ,gNextMove = O
       ,gMinGameStake = minGameStake
       ,gStake = 10_000_000
       ,gDeadline = slotToBeginPOSIXTime def 10
       ,gCurrency = currSymbol
       ,gTokenName = tName
       ,gState = initGameState
      }
    -- void $ Trace.waitNSlots 1    
    s <- Trace.waitNSlots 1
    Extras.logInfo $ "reached " ++ Haskell.show s

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"move" h1 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Nothing
        ,mSecondPlayerMarker  = Nothing
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play O
      }
    void $ Trace.waitNSlots 1

currSymbol :: CurrencySymbol
currSymbol = currencySymbol "dcddcaa"

tName :: TokenName
tName = tokenName "T1"

tName2 :: TokenName
tName2 = tokenName "T2"

-- Create a token of AssetClass to send it to the wallet
token :: AssetClass
token = AssetClass (currSymbol, tName)

test1 :: IO ()
test1 = Trace.runEmulatorTraceIO' traceConfig emulatorConfig myTraceTest
-- test1 = Trace.runEmulatorTraceIO' def def myTraceTest