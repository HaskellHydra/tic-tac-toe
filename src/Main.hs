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
import Plutus.Trace as Trace
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

data Game = Game
    { tFirstPlayer        :: !(BuiltinByteString, PaymentPubKeyHash)
    , tSecondPlayer       :: !(Maybe (BuiltinByteString, PaymentPubKeyHash))
    , tGameStake          :: !Integer
    , tDeadline           :: !POSIXTime
    , identifierToken     :: !AssetClass
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

data GameChoice = X | O | N
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    X == X = True
    O == O = True
    N == N = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''GameChoice

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
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

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
mkValidator datum redeemer ctx = (traceIfFalse "Wrong datum" checkCorrectDatum ) &&
                                 (traceIfFalse "Deadline expired! (or) Unable to extract datum" checkDeadline)                                 
    where
        checkDeadline :: Bool
        checkDeadline = contains (from $ tDeadline $ gdGame datum) $ txInfoValidRange $ scriptContextTxInfo ctx

        checkCorrectDatum :: Bool
        checkCorrectDatum = True

    ------------------------------
    -- TODO: In the below code check if the `findDatum h info` is extracting the datum from the output.
    ------------------------------

    -- ownOutput   :: TxOut
    -- outputDatum :: AuctionDatum
    -- (ownOutput, outputDatum) = case getContinuingOutputs ctx of
    --     [o] -> case txOutDatumHash o of
    --         Nothing   -> traceError "wrong output type"
    --         Just h -> case findDatum h info of
    --             Nothing        -> traceError "datum not found"
    --             Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
    --                 Just ad' -> (o, ad')
    --                 Nothing  -> traceError "error decoding data"
    --     _   -> traceError "expected exactly one continuing output"

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

-- Start Game Params
data StartParams = StartParams {
    gFirstPlayerPkh      :: !PaymentPubKeyHash
   ,gFirstPlayerMarker   :: !BuiltinByteString
   ,gSecondPlayerPkh     :: !(Maybe PaymentPubKeyHash)
   ,gSecondPlayerMarker  :: !(Maybe BuiltinByteString)
   ,gStake               :: !Integer
   ,gDeadline            :: !POSIXTime
   ,gCurrency             :: !CurrencySymbol
   ,gTokenName           :: !TokenName
   ,gState               :: !GameState
} deriving (Generic, ToJSON, FromJSON)

-- | The schema of the contract, with one endpoint to publish the problem with a bounty and another to sbumit solutions
type TicTacToeSchema = Endpoint  "start" StartParams
                       .\/ Endpoint "test" ()

start :: AsContractError e => StartParams -> Contract w s e ()
start StartParams{..} = do
    logInfo @Haskell.String "---------------- Start Game -------------------------"  
    pkh <- ownPaymentPubKeyHash
    let g = Game
              {
                tFirstPlayer = (gFirstPlayerMarker, gFirstPlayerPkh)
               ,tSecondPlayer = Nothing
               ,tGameStake = gStake
               ,tDeadline = gDeadline
               ,identifierToken = AssetClass (gCurrency, gTokenName)
              }       
        d = GameDatum
            {
                gdGame = g
               ,gdGameState = gState
            }
        v = Value.singleton gCurrency gTokenName 1 <> Ada.lovelaceValueOf minLovelace    
        -- v = (Value.assetClassValue (Value.AssetClass (gCurrency, gTokenName)) 1) <> Ada.lovelaceValueOf minLovelace
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints ticTacToeInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Haskell.String $ printf "Started game with paramters %s and token %s" (Haskell.show g) (Haskell.show v)

test :: forall w s e. AsContractError e => Contract w s e ()
test = do
    logInfo @Haskell.String "-----------Printing test logs---------"
    unspentOutputs <- utxosAt gameAddress
    -- 'collectFromScript' is a function of the wallet API. It creates a tx consuming
    -- all unspent transaction outputs at a script address and pays them to a
    -- public key address owned by this wallet.
    logInfo @Haskell.String $ Haskell.show $ Map.toList unspentOutputs


-- | TicTacToe endpoints.
endpoints :: forall  e. AsContractError e => Contract () TicTacToeSchema e ()
endpoints = awaitPromise (start' `select` test') >> endpoints
  where
    start' = endpoint @"start" start
    test' = endpoint @"test" $ const test

-- mkSchemaDefinitions ''TicTacToeSchema

emulatorConfig :: Trace.EmulatorConfig
emulatorConfig =
  Trace.EmulatorConfig
    (Left $ Map.fromList [(knownWallet 1, Ada.lovelaceValueOf 30_000_000),
                          (knownWallet 2, Ada.lovelaceValueOf 10_000_000)])
    def
    def

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
    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints @ContractError
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints @ContractError
    Trace.callEndpoint @"test" h1 ()
    s <- Trace.waitNSlots 1

    -- Second player has not joined the game yet.
    Trace.callEndpoint @"start" h1 $ StartParams{
        gFirstPlayerPkh = mockWalletPaymentPubKeyHash (knownWallet 1)
       ,gFirstPlayerMarker = (BuiltinByteString Haskell.. C.pack) "X"
       ,gSecondPlayerPkh = Nothing -- mockWalletPaymentPubKeyHash (knownWallet 2)
       ,gSecondPlayerMarker = Nothing -- (BuiltinByteString Haskell.. C.pack) "O"
       ,gStake = 10_000_000
       ,gDeadline = slotToBeginPOSIXTime def 10
       ,gCurrency = currSymbol
       ,gTokenName = tName
       ,gState = initGameState
    }
    s <- Trace.waitNSlots 1
    Extras.logInfo $ "reached " ++ Haskell.show s


currSymbol :: CurrencySymbol
currSymbol = currencySymbol "dcddcaa"

tName :: TokenName
tName = tokenName "T1"

test1 :: IO ()
-- test1 = Trace.runEmulatorTraceIO' traceConfig emulatorConfig myTraceTest
test1 = Trace.runEmulatorTraceIO' def def myTraceTest