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
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Default               (def, Default (..))
import           Data.Text            (pack, Text)
import Ledger (Address, dataHash ,Datum (Datum), DatumHash (..), ScriptContext, Validator, Value, getCardanoTxId, valuePaidTo)
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

data GameChoice = X | O 
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    X == X = True
    O == O = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''GameChoice

data Game = Game
    { tFirstPlayer        :: !(BuiltinByteString, PaymentPubKeyHash)
    , tSecondPlayer       :: !(Maybe (BuiltinByteString, PaymentPubKeyHash))
    , tGameStake          :: !Integer
    , tMinGameStake       :: !Integer -- This value can be hardcoded
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

data GameState = GameState Integer
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

data GameRedeemer = Play Integer GameChoice | ClaimFirst | ClaimSecond 
    deriving (Show, Generic, ToJSON, FromJSON, Haskell.Eq)

instance Eq GameRedeemer where
    {-# INLINABLE (==) #-}
    (Play s X) == (Play s' X) = ((X == X) && (s == s'))
    (Play s O) == (Play s' O) = ((O == O) && (s == s'))
    ClaimFirst == ClaimFirst = True
    ClaimSecond == ClaimSecond = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE replaceDigits #-}
replaceDigits :: Integer -> Integer -> Integer -> Integer
replaceDigits x p n | (p == 1) =  (\z -> n + (x - z) ) (extractDigits x p)
                    | (p == 9) =  (\z -> (n*100_000_000) + (x - (z*100_000_000)) )  (extractDigits x p)
                    | (p == 8) =  (\z -> (n*10_000_000) + (x - (z*10_000_000)) )  (extractDigits x p)
                    | (p == 7) =  (\z -> (n*1_000_000) + (x - (z*1_000_000)) )  (extractDigits x p)
                    | (p == 6) =  (\z -> (n*100_000) + (x - (z*100_000)) )  (extractDigits x p)
                    | (p == 5) =  (\z -> (n*10_000) + (x - (z*10_000)) )  (extractDigits x p)
                    | (p == 4) =  (\z -> (n*1_000) + (x - (z*1_000)) )  (extractDigits x p)
                    | (p == 3) =  (\z -> (n*100) + (x - (z*100)) )  (extractDigits x p)
                    | (p == 2) =  (\z -> (n*10) + (x - (z*10)) )  (extractDigits x p)
                    | otherwise = -1000


{-# INLINABLE extractDigits #-}
extractDigits :: Integer -> Integer -> Integer
extractDigits x p = if (p == 1) then 
                       snd $ PlutusTx.Prelude.divMod x 10 
                     else if (p == 9) then
                       fst $ PlutusTx.Prelude.divMod x 100_000_000
                     else if (p == 8) then
                       snd $ PlutusTx.Prelude.divMod (fst $ PlutusTx.Prelude.divMod x 10_000_000) 10
                     else if (p == 7) then
                       snd $ PlutusTx.Prelude.divMod (fst $ PlutusTx.Prelude.divMod x 1_000_000) 10
                     else if (p == 6) then
                       snd $ PlutusTx.Prelude.divMod (fst $ PlutusTx.Prelude.divMod x 100_000) 10                                              
                     else if (p == 5) then
                       snd $ PlutusTx.Prelude.divMod (fst $ PlutusTx.Prelude.divMod x 10_000) 10
                     else if (p == 4) then
                       snd $ PlutusTx.Prelude.divMod (fst $ PlutusTx.Prelude.divMod x 1_000) 10                                              
                     else if (p == 3) then
                       snd $ PlutusTx.Prelude.divMod (fst $ PlutusTx.Prelude.divMod x 100) 10
                     else if (p == 2) then
                       snd $ PlutusTx.Prelude.divMod (fst $ PlutusTx.Prelude.divMod x 10) 10
                     else
                       -1000 -- since the maximum value that we compare is only 111, a failure will always result in a number < -1000 
-- ---------->
-- R1  R2  R3
-- 555_555_555
-- j = [ extractDigits x i | i<- reverse [1..9]]

{-# INLINABLE checkTie #-} -- None of the positions are a 5
checkTie :: GameState -> Integer -> Bool -> Bool
checkTie (GameState x) n t | (n > 0) = checkTie (GameState x) (n-1) ( ((extractDigits x n) == 5) || t )
               | otherwise = not t

{-# INLINABLE checkRows #-}
checkRows :: Integer -> Integer -> Bool
checkRows x c = let r1 = 100 * (extractDigits x 9 ) + ( 10* (extractDigits x 8)) + (extractDigits x 7)
                    r2 = 100 * (extractDigits x 6 ) + ( 10* (extractDigits x 5)) + (extractDigits x 4)
                    r3 = 100 * (extractDigits x 3 ) + ( 10* (extractDigits x 2)) + (extractDigits x 1)
                in
                  if (c == 1) then
                    (r1 == 111) || (r2 == 111) || (r3 == 111)
                  else if ( c == 0) then
                    (r1 == 0) || (r2 == 0) || (r3 == 0)
                  else
                    False


{-# INLINABLE checkCols #-}
checkCols :: Integer -> Integer -> Bool
checkCols x c = let c1 = 100 * (extractDigits x 9 ) + ( 10* (extractDigits x 6)) + (extractDigits x 3)
                    c2 = 100 * (extractDigits x 8 ) + ( 10* (extractDigits x 5)) + (extractDigits x 2)
                    c3 = 100 * (extractDigits x 7 ) + ( 10* (extractDigits x 4)) + (extractDigits x 1)
                in
                  if (c == 1) then
                    (c1 == 111) || (c2 == 111) || (c3 == 111)
                  else if ( c == 0) then
                    (c1 == 0) || (c2 == 0) || (c3 == 0)
                  else
                    False


{-# INLINABLE checkDiags #-}
checkDiags :: Integer -> Integer -> Bool
checkDiags x c = let d1 = 100 * (extractDigits x 9 ) + ( 10* (extractDigits x 5)) + (extractDigits x 1)
                     d2 = 100 * (extractDigits x 7 ) + ( 10* (extractDigits x 5)) + (extractDigits x 3)
                 in
                   if (c == 1) then
                     (d1 == 111) || (d2 == 111)
                   else if ( c == 0) then
                     (d1 == 0) || (d2 == 0)
                   else
                     False


{-# INLINABLE initGameState #-}
initGameState :: GameState
initGameState = GameState 555_555_555


{-# INLINABLE mkValidator #-}
mkValidator :: GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkValidator datum redeemer ctx = (traceIfFalse "Wrong game marker selection by the player" evalGameTx ) &&
                                 (traceIfFalse "Player stake does not match" $ checkGameStake ) && 
                                 (traceIfFalse "Wrong Datum" checkCorrectDatum) &&
                                 checkMinStake   
                                 -- moved to evalGame 
                                   -- (traceIfFalse "Deadline expired! (or) Unable to extract datum" checkDeadline) &&
    where

        checkDeadline :: Bool
        checkDeadline = contains (to $ tDeadline $ gdGame datum) (txInfoValidRange $ scriptContextTxInfo ctx)

        -- If the second player is nothing in the output datum and the if the input is 'Just x' it is false
        -- If the second player is nothing both in input and output datums then player 2 did not join the game
          -- The game stake is only 1* gamestake      


        -- ClaimFirst and ClaimSecond are independent of the checkDeadline
        -- The deadline will be checked in checkGameStake and when a player 
           -- wants to claim after the deadline the script will check if the game was won 
           -- or if its a tie. Depending upon the condition it will 
           -- pay 2*gamestake or just 1*gamestake to the player claiming the prize.
        -- The below code can be obsoarbed into the checkGameStake 
        evalGameTx :: Bool
        evalGameTx = case redeemer of
            Play loc c -> (traceIfFalse "Its a tie!!" $ not $ checkTie inGameState 9 False ) &&
                          (traceIfFalse "Game Won !!" $ not $ (winner inGameState 1) || (winner inGameState 0) ) &&
                          (traceIfFalse "Deadline expired! (or) Unable to extract datum" checkDeadline) &&
                          (checkPlayerChoice c) && 
                          (checkGame inGameState redeemer)                          
            ClaimFirst -> True -- Setting this True check is being done in the checkGameStake
            ClaimSecond -> True -- Setting this True check is being done in the checkGameStake
            _ -> False
          
        checkGame :: GameState -> GameRedeemer -> Bool 
        checkGame gS@(GameState x)  gR@(Play loc c) = let playerChoice = if (c == X) then 1 
                                                                         else if (c== O) then 0
                                                                         else traceError "Wrong choice!"
                                                      in
                                                        outGameState == (GameState $ expMove playerChoice)  
                                                        where
                                                          expMove :: Integer -> Integer
                                                          expMove playerChoice = if ((extractDigits x loc) == 5 ) then 
                                                                                   replaceDigits x loc playerChoice
                                                                                 else 
                                                                                   traceError "Position is already filled by other player!"
                                                          
        winner :: GameState -> Integer -> Bool
        winner gS@(GameState x) c = (checkRows x c) || (checkCols x c) || (checkDiags x c)

                              
        checkPlayerChoice :: GameChoice -> Bool
        checkPlayerChoice c =  if (c == X) then
                                ( traceIfFalse "Not signed by Player1" $ txSignedBy info $ unPaymentPubKeyHash $ snd $ tFirstPlayer inGameCfg ) && 
                                ( traceIfFalse "Player1 trying to tamper with the next move marker !" $ c /= (nextMove outGameCfg)) &&
                                ( traceIfFalse "Player is making a double move" $ c == (nextMove inGameCfg) )
                              else if ( c == O ) then
                                case (tSecondPlayer outGameCfg) of
                                  Nothing -> False 
                                  Just x -> ( traceIfFalse "Not signed by Player2" $ txSignedBy info $ unPaymentPubKeyHash $ snd x ) &&                                                       
                                            ( traceIfFalse "Player2 trying to tamper with the next move marker !" $ c /= (nextMove outGameCfg)) &&
                                            ( traceIfFalse "Player is making a double move" $ c == (nextMove inGameCfg) )
                              else
                                  traceError "Wrong game choice"

---------------------------------------------------------------------------------------------------------------------------------
-- Use this function (Ada.getLovelace . Ada.fromValue) to get only the ADA/lovelaces [check the deifinition for 'lovelaces']
-- Prelude Ledger Value Ada Main> (Ada.getLovelace . Ada.fromValue) (Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 1)
-- 100000000
-- Prelude Ledger Value Ada Main> (Ada.fromValue) (Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 1)
-- Lovelace {getLovelace = 100000000} 
---------------------------------------------------------------------------------------------------------------------------------

        checkGameStake :: Bool
        checkGameStake = case ( redeemer ,tSecondPlayer inGameCfg, tSecondPlayer outGameCfg ) of
            -- When a second player joins a game we need to verify if the player meets the minimum stake
            -- Also check the initial game state it should match 555_555_555 
            -- and the output TX has a 2*gamestake, In this casse the TX will be signed by the player2 
                            (Play _ _, Nothing, Just (_, ppkh2)) -> 
                                (traceIfFalse "Wrong input values !!!" $ checkInval 1) &&
                                (traceIfFalse "Initial game state does not match!" $ inGameState == (GameState 555_555_555) ) && -- Player 1 might be cheating
                                (traceIfFalse "Not signed by player2" $ txSignedBy info $ unPaymentPubKeyHash ppkh2) &&
                                (traceIfFalse "The second player stake does not match" $ checkOutVal 2 )  --(lovelaces $ txOutValue ownOutput) == 2*(tGameStake outGameCfg) )
            -- In this case only the 1stPlayer stake exists in the script and the second Player has not joined the game yet
            -- This case is not hit, since there is a check to prevent a double move by a player
                            (Play _ _, Nothing, Nothing) -> 
                                (traceIfFalse "Wrong input values !!!" $ checkInval 1) &&
                                (traceIfFalse "Not signed by player1" $ txSignedBy info $ unPaymentPubKeyHash $ snd $ tFirstPlayer outGameCfg ) &&
                                (traceIfTrue "In checkGameStake" True) &&                               
                                (traceIfFalse "The First player stake does not match" $  checkOutVal 1 ) -- (lovelaces $ txOutValue ownOutput) == (tGameStake outGameCfg) )
            -- Prevent players from stealing the game stake in the script 
                            (Play _ _, Just _, Just (_, ppkh2)) -> 
                                (traceIfFalse "Wrong input values !!!" $ checkInval 2) &&
                                (traceIfFalse 
                                "Not signed by any of the players participating in the game" 
                                $ (txSignedBy info $ unPaymentPubKeyHash $ snd $ tFirstPlayer outGameCfg) || (txSignedBy info $ unPaymentPubKeyHash ppkh2) ) &&
                                (traceIfFalse "The player stake does not match [Game started with both stakes]" $ checkOutVal 2 ) -- (lovelaces $ txOutValue ownOutput) == 2*(tGameStake outGameCfg) )
                            (ClaimSecond, Just _, Just (_, ppkh2)) -> 
                                case ((winner inGameState 0), (checkTie inGameState 9 False),checkDeadline) of
                                  (True, False, _) -> 
                                    (traceIfFalse "Wrong input values !!!" $ checkInval 2) &&
                                    (traceIfFalse 
                                    "Win:Not signed by the second player" 
                                    $ (txSignedBy info $ unPaymentPubKeyHash ppkh2) ) &&
                                    (traceIfFalse "Win:Wrong amount is being paid to the player2" $ checkPlayerOutput (unPaymentPubKeyHash ppkh2) 2)
                                  (False, True, _) -> 
                                    -- (traceIfFalse "Wrong input values !!!" $ checkInval 2) && -- input might be 1*gamestake
                                    (traceIfFalse 
                                    "Tie:Not signed by the second player" 
                                    $ (txSignedBy info $ unPaymentPubKeyHash ppkh2) ) &&
                                    (traceIfFalse "Tie:Wrong amount is being paid to the player2" $ checkPlayerOutput (unPaymentPubKeyHash ppkh2) 1)
                                  (False, False, True) ->
                                    traceError "Cannot claim the prize while the game is in progress!"
                                  (False, False, False) ->
                                    (traceIfFalse 
                                    "Not signed by the second player" 
                                    $ (txSignedBy info $ unPaymentPubKeyHash ppkh2) ) &&
                                    (traceIfFalse "Wrong amount is being paid to the player2" $ checkPlayerOutput (unPaymentPubKeyHash ppkh2) 1)
                                  (_,_,_) ->
                                    traceError "Unknown state while claiming prize!"

                            (ClaimFirst, Just _, Just (_, ppkh2)) -> 
                                case ((winner inGameState 1), (checkTie inGameState 9 False),checkDeadline) of
                                  (True, _, _) -> 
                                    (traceIfFalse "Wrong input values !!!" $ checkInval 2) &&
                                    (traceIfFalse 
                                    "Not signed by the first player" 
                                    $ (txSignedBy info $ unPaymentPubKeyHash $ snd $ tFirstPlayer outGameCfg)  ) &&                                
                                    (traceIfFalse "Wrong amount is being paid to the player1" $ checkPlayerOutput (unPaymentPubKeyHash $ snd $ tFirstPlayer inGameCfg) 2)
                                  (_, True, _) -> 
                                    -- (traceIfFalse "Wrong input values !!!" $ checkInval 2) && -- input might be 1*gamestake
                                    (traceIfFalse 
                                    "Not signed by the first player" 
                                    $ (txSignedBy info $ unPaymentPubKeyHash $ snd $ tFirstPlayer outGameCfg)  ) && 
                                    (traceIfFalse "Wrong amount is being paid to the player1" $ checkPlayerOutput (unPaymentPubKeyHash $ snd $ tFirstPlayer inGameCfg) 1)
                                  (False, False, True) ->
                                    traceError "Cannot claim the prize while the game is in progress!"
                                  (False, False, False) ->
                                    (traceIfFalse 
                                    "Not signed by the first player" 
                                    $ (txSignedBy info $ unPaymentPubKeyHash $ snd $ tFirstPlayer outGameCfg)  ) && 
                                    (traceIfFalse "Wrong amount is being paid to the player1" $ checkPlayerOutput (unPaymentPubKeyHash $ snd $ tFirstPlayer inGameCfg) 1)
                                  (_,_,_) ->
                                    traceError "Unknown state while claiming prize!"

                            (_,_, _) -> (traceIfFalse "Unknown stake parameters " False) 

        checkMinStake :: Bool
        checkMinStake = traceIfFalse "Min game stake not met!!" $ (tGameStake outGameCfg) >= minGameStake                           

        -- The following fields - tSecondPlayer, nextMove keep changing
        -- and the second player config will change from Nothing to Just (location, ppkh) after making the first move 
        -- Note: The parameters that do not change can be made as parameters to the contract and must be sperated 
        --       from the datum fields that change for easier comparison
        checkCorrectDatum :: Bool
        checkCorrectDatum = 
            case ( tSecondPlayer inGameCfg, tSecondPlayer outGameCfg ) of
                (Nothing, _) -> ((tFirstPlayer inGameCfg) == (tFirstPlayer outGameCfg)) &&
                                ((tGameStake inGameCfg) == (tGameStake outGameCfg)) &&
                                ((tMinGameStake inGameCfg) == (tMinGameStake outGameCfg)) &&
                                ((tDeadline inGameCfg)==(tDeadline outGameCfg)) &&
                                ((identifierToken inGameCfg)==(identifierToken outGameCfg)) &&
                                (((nextMove inGameCfg) == X) || ((nextMove inGameCfg) == O) ) &&
                                (((nextMove outGameCfg) == X) || ((nextMove outGameCfg) == O) ) 

                (Just x, Just y) -> ((tFirstPlayer inGameCfg) == (tFirstPlayer outGameCfg)) &&
                                    ((tGameStake inGameCfg) == (tGameStake outGameCfg)) &&
                                    ((tMinGameStake inGameCfg) == (tMinGameStake outGameCfg)) &&
                                    ((tDeadline inGameCfg)==(tDeadline outGameCfg)) &&
                                    ((identifierToken inGameCfg)==(identifierToken outGameCfg)) &&
                                    (((nextMove inGameCfg) == X) || ((nextMove inGameCfg) == O) ) &&
                                    (((nextMove outGameCfg) == X) || ((nextMove outGameCfg) == O) ) &&                                    
                                    (x == y)
                (_, _)       -> False         

        ------------------------
        -- TX helper functions
        ------------------------

        getScriptInput :: TxInInfo
        getScriptInput = 
          let xs = [i | i <- txInfoInputs info, Data.Maybe.isJust $ ( txOutDatumHash . txInInfoResolved ) i ]
          in
            case xs of
              [i] -> i
              _   -> traceError "expected exactly one script input" 

        checkOutVal :: Integer -> Bool
        checkOutVal x = let expValue = (assetClassValue (identifierToken outGameCfg) 1) <> (Ada.lovelaceValueOf $ x * (tGameStake outGameCfg)) 
                            curValue = (txOutValue ownOutput)
                        in
                          expValue == curValue

        -- When player 2 joins the game the GameStake will change to 2x
        -- This function will also check that the script TX contains the token in it
        checkInval :: Integer -> Bool
        checkInval x =  let expValue = (assetClassValue (identifierToken inGameCfg) 1) <> (Ada.lovelaceValueOf $ x * (tGameStake inGameCfg)) 
                            curValue = (txOutValue . txInInfoResolved $ getScriptInput)
                        in
                          expValue == curValue

        -- Check how much stake is being paid to the players
        -- The below function will only check if a output to the ppkh exists 
          -- Still need to verify if this output is coming from the script
        checkPlayerOutput :: PubKeyHash -> Integer -> Bool
        checkPlayerOutput ppkh n = elem (Ada.lovelaceValueOf $ n * (tGameStake inGameCfg)) (pubKeyOutputsAt ppkh info)        
        -- checkPlayerOutput ppkh n = let v = (Ada.getLovelace . Ada.fromValue) (valuePaidTo info ppkh)
        --                            in
        --                              v == (n * (tGameStake inGameCfg))

        -- checkPlayerOutput ppkh n = let outputs = txInfoOutputs
        --                                valHash = ownHash ctx
        --                            in
        --                              length (scriptOutputsAt valHash info) == 1

        -- checkPlayerOutput ppkh n = True
        -- checkPlayerOutput ppkh n = let v = (Ada.getLovelace . Ada.fromValue) (valuePaidTo info ppkh)
        --                            in
        --                              v > 1007949476 -- (2 * (tGameStake inGameCfg))

        -- checkPlayerOutput ppkh n = case (pubKeyOutputsAt ppkh info) of
        --                              [v] -> v == (Ada.lovelaceValueOf $ n * (tGameStake inGameCfg)) 
        --                              _   -> False

        inGameCfg :: Game
        inGameCfg = gdGame datum

        outGameCfg :: Game
        outGameCfg = gdGame outputDatum

        inGameState :: GameState
        inGameState = gdGameState datum

        outGameState :: GameState
        outGameState = gdGameState outputDatum


                       
        info :: TxInfo    
        info = scriptContextTxInfo ctx

        ownInput :: TxOut
        ownInput = case (findOwnInput ctx) of
                    Nothing -> traceError "game input is missing"
                    Just x -> txInInfoResolved x

        ownOutput   :: TxOut
        outputDatum :: GameDatum
        (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        -- use the 'getContinuingOutputs' function carefully when the end case happens we need to transfer everything from the script to the winner
        -- For the close case you need to avoid using this function
            [o] -> case txOutDatumHash o of
                Nothing   -> traceError "wrong output type"
                Just h -> case findDatum h info of
                    Nothing        -> traceError "datum not found"
                    Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                        Just ad' -> (o, ad')
                        Nothing  -> traceError "error decoding data"
            _   -> traceError "expected exactly one continuing output"


        ownOutputPpkh :: Maybe PaymentPubKeyHash
        ownOutputPpkh =  PaymentPubKeyHash Haskell.<$> (toPubKeyHash $ txOutAddress ownOutput)


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
                       .\/ Endpoint "move2" MoveParams
                       .\/ Endpoint "claimFirst" MoveParams
                       .\/ Endpoint "claimSnd" MoveParams                       
                       .\/ Endpoint "claimTieSnd" MoveParams                                              
                       .\/ Endpoint "playGame" MoveParams
                       .\/ Endpoint "moveS" MoveParams
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
        -- v = (Ada.lovelaceValueOf gStake )        
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
    let d' = buildDatum
          where buildDatum = case (mSecondPlayerMarker, mSecondPlayerPkh) of
                         
                         (Just mSndPlayerMarker, Just mSndPlayerPkh) -> d { 
                           gdGame = gdGame { nextMove = mNextMove
                           ,tSecondPlayer = Just (mSndPlayerMarker,mSndPlayerPkh)} }

                         (_, _) -> d { gdGame = gdGame { nextMove = mNextMove} }

        r = Redeemer $ PlutusTx.toBuiltinData mChoice 
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

mkMoveV2 :: MoveParams -> Contract w s Text ()
mkMoveV2 MoveParams{..} = do
    logInfo @Haskell.String "-------------- Make a Move ---------------"
    (oref, o, d@GameDatum{..}) <- findGameDatum mCurrency mTokenName
    logInfo @Haskell.String $ printf "Datum from findDatum %s" (Haskell.show $ d)
    logInfo @Haskell.String $ printf "Using the next move as : %s" (Haskell.show mNextMove)
    let d' = buildDatum
          where buildDatum = case (mSecondPlayerMarker, mSecondPlayerPkh) of
                         
                         (Just mSndPlayerMarker, Just mSndPlayerPkh) -> d { 
                           gdGame = gdGame { nextMove = mNextMove
                           ,tSecondPlayer = Just (mSndPlayerMarker,mSndPlayerPkh)} }

                         (_, _) -> d { gdGame = gdGame { nextMove = mNextMove} }

        r = Redeemer $ PlutusTx.toBuiltinData mChoice 
        v = (Value.singleton mCurrency mTokenName 1) <> (Ada.lovelaceValueOf mStake ) <> (Ada.lovelaceValueOf mStake )
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

playGame :: MoveParams -> Contract w s Text ()
playGame MoveParams{..} = do
    logInfo @Haskell.String "-------------- Make a Move ---------------"
    (oref, o, d@GameDatum{..}) <- findGameDatum mCurrency mTokenName -- We have access to all the records inside d(GameDatum) you can access them directly
    logInfo @Haskell.String $ printf "Datum from findDatum %s" (Haskell.show $ d)
    logInfo @Haskell.String $ printf "Using the next move as : %s" (Haskell.show mNextMove)
    -- logInfo @Haskell.String $ printf "GameState: %s" (Haskell.show (gdGameState) )

    let d' = buildDatum
          where buildDatum = case (mSecondPlayerMarker, mSecondPlayerPkh) of
                         
                         (Just mSndPlayerMarker, Just mSndPlayerPkh) -> d { 
                           gdGame = gdGame { nextMove = mNextMove
                           ,tSecondPlayer = Just (mSndPlayerMarker,mSndPlayerPkh)},
                           gdGameState =  modGameState gdGameState mChoice
                            } 

                         (_, _) -> d { gdGame = gdGame { nextMove = mNextMove} }
                
                modGameState :: GameState -> GameRedeemer -> GameState
                modGameState (GameState x) (Play loc c)  =  let playerChoice = if (c == X) then 1 
                                                                               else if (c== O) then 0
                                                                               else traceError "Wrong choice!"
                                                            in
                                                              GameState $ replaceDigits x loc playerChoice


        r = Redeemer $ PlutusTx.toBuiltinData mChoice 
        v = (Value.singleton mCurrency mTokenName 1) <> (Ada.lovelaceValueOf mStake ) <> (Ada.lovelaceValueOf mStake )
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


-- Move to steal the token
mkMoveS :: MoveParams -> Contract w s Text ()
mkMoveS MoveParams{..} = do
    logInfo @Haskell.String "-------------- Make a Move ---------------"
    (oref, o, d@GameDatum{..}) <- findGameDatum' mCurrency mTokenName
    logInfo @Haskell.String $ printf "Datum from findDatum %s" (Haskell.show $ d)
    logInfo @Haskell.String $ printf "Using the next move as : %s" (Haskell.show mNextMove)
    let d' = buildDatum
          where buildDatum = case (mSecondPlayerMarker, mSecondPlayerPkh) of
                         
                         (Just mSndPlayerMarker, Just mSndPlayerPkh) -> d { 
                           gdGame = gdGame { nextMove = mNextMove
                           ,tSecondPlayer = Just (mSndPlayerMarker,mSndPlayerPkh)} }

                         (_, _) -> d { gdGame = gdGame { nextMove = mNextMove} }

        r = Redeemer $ PlutusTx.toBuiltinData mChoice 
        -- v = (Value.singleton mCurrency mTokenName 1) <> (Ada.lovelaceValueOf mStake )
        v = (Ada.lovelaceValueOf mStake ) <> (Ada.lovelaceValueOf mStake )
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

claimFirst :: MoveParams -> Contract w s Text ()
claimFirst MoveParams{..} = do
    logInfo @Haskell.String "-------------- Claim First ---------------"
    (oref, o, d@GameDatum{..}) <- findGameDatum mCurrency mTokenName
    logInfo @Haskell.String $ printf "Datum from findDatum %s" (Haskell.show $ d)    

    let r = Redeemer $ PlutusTx.toBuiltinData mChoice 
        v = (Value.singleton mCurrency mTokenName 1) <> (Ada.lovelaceValueOf mStake ) <> (Ada.lovelaceValueOf mStake ) -- send everything to the winner
        -- explained here https://cardano.stackexchange.com/questions/2296/lecture-6-it-2-core-hs-explaining-lookups-use-of-both-typedvalidatorlook
        lookups = Constraints.typedValidatorLookups ticTacToeInstance Haskell.<> -- used for the output utxo with the new contract instance
                  Constraints.otherScript validator                   Haskell.<> -- used for consuming the input contract instance
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx = Constraints.mustPayToPubKey ( snd $ tFirstPlayer gdGame) v <> -- transfer the token and the stake to player2
             Constraints.mustValidateIn (to $ tDeadline gdGame)          <>
             Constraints.mustSpendScriptOutput oref r

    logInfo @Haskell.String $ printf "The value %s has been sent to the Player1" (Haskell.show v)
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx


claimSnd :: MoveParams -> Contract w s Text ()
claimSnd MoveParams{..} = do
    logInfo @Haskell.String "-------------- Claim Second ---------------"
    (oref, o, d@GameDatum{..}) <- findGameDatum mCurrency mTokenName
    logInfo @Haskell.String $ printf "Datum from findDatum %s" (Haskell.show $ d)    

    let r = Redeemer $ PlutusTx.toBuiltinData mChoice 
        v_player = (Ada.lovelaceValueOf mStake ) <> (Ada.lovelaceValueOf mStake ) -- send everything to the winner
        v_script = (Value.singleton mCurrency mTokenName 1) <> (Ada.lovelaceValueOf 2_000_000 ) -- The on-chain code is making use of continuing script outputs everytime and if script does not hold the token it will fail
        -- explained here https://cardano.stackexchange.com/questions/2296/lecture-6-it-2-core-hs-explaining-lookups-use-of-both-typedvalidatorlook
        lookups = Constraints.typedValidatorLookups ticTacToeInstance Haskell.<> -- used for the output utxo with the new contract instance
                  Constraints.otherScript validator                   Haskell.<> -- used for consuming the input contract instance
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx = Constraints.mustPayToPubKey ( snd $ fromJust $ tSecondPlayer gdGame) v_player <> -- transfer the token and the stake to player2
             Constraints.mustPayToTheScript d v_script                            <>        
             Constraints.mustValidateIn (to $ tDeadline gdGame)          <>
            --  Constraints.mustValidateIn (from $ tDeadline gdGame)          <>   -- Uncomment to claim after the deadline          
             Constraints.mustSpendScriptOutput oref r

    logInfo @Haskell.String $ printf "The value %s has been sent to the Player2" (Haskell.show v_player)
    ledgerTx <- submitTxConstraintsWith lookups tx
    logInfo @Haskell.String $ printf "TX for player 2 %s" (Haskell.show ledgerTx)
    -- logInfo @Haskell.String $ printf "Utxos at player 2 address %s" (Haskell.show $ pubKeyOutputsAt (unPaymentPubKeyHash $ snd $ fromJust $ tSecondPlayer gdGame))    
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx


claimTieSnd :: MoveParams -> Contract w s Text ()
claimTieSnd MoveParams{..} = do
    logInfo @Haskell.String "-------------- Claim Tie ---------------"
    (oref, o, d@GameDatum{..}) <- findGameDatum mCurrency mTokenName
    logInfo @Haskell.String $ printf "Datum from findDatum %s" (Haskell.show $ d)    

    let r = Redeemer $ PlutusTx.toBuiltinData mChoice 
        v_player = (Ada.lovelaceValueOf mStake )  -- send everything to the winner
        v_script = (Value.singleton mCurrency mTokenName 1) <> (Ada.lovelaceValueOf mStake ) -- The on-chain code is making use of continuing script outputs everytime and if script does not hold the token it will fail
        -- explained here https://cardano.stackexchange.com/questions/2296/lecture-6-it-2-core-hs-explaining-lookups-use-of-both-typedvalidatorlook
        lookups = Constraints.typedValidatorLookups ticTacToeInstance Haskell.<> -- used for the output utxo with the new contract instance
                  Constraints.otherScript validator                   Haskell.<> -- used for consuming the input contract instance
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx = Constraints.mustPayToPubKey ( snd $ fromJust $ tSecondPlayer gdGame) v_player <> -- transfer the token and the stake to player2
             Constraints.mustPayToTheScript d v_script                            <>        
             Constraints.mustValidateIn (to $ tDeadline gdGame)          <> 
            -- Constraints.mustValidateIn (from $ tDeadline gdGame)          <> -- Uncomment this when trying to claim after the deadline
             Constraints.mustSpendScriptOutput oref r

    logInfo @Haskell.String $ printf "The value %s has been sent to the Player2" (Haskell.show v_player)
    ledgerTx <- submitTxConstraintsWith lookups tx
    logInfo @Haskell.String $ printf "TX for player 2 %s" (Haskell.show ledgerTx)
    -- logInfo @Haskell.String $ printf "Utxos at player 2 address %s" (Haskell.show $ pubKeyOutputsAt (unPaymentPubKeyHash $ snd $ fromJust $ tSecondPlayer gdGame))    
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx



-- Just for testing submit datum with token
findGameDatum' :: CurrencySymbol -> TokenName -> Contract w s Text (TxOutRef, ChainIndexTxOut, GameDatum)
findGameDatum' cs tn = do
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
                Just d@GameDatum{..} -> return (oref, o, d)
                    -- | AssetClass(cs, tn) == (identifierToken gdGame) -> return (oref, o, d)
                    -- | otherwise                                           -> throwError "game token missmatch"
        _           -> throwError "game utxo not found"



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
endpoints = awaitPromise (start' `select` 
                          move' `select` 
                          move2' `select` 
                          claimFirst' `select` 
                          claimSnd' `select` 
                          claimTieSnd' `select`
                          moveS' `select` 
                          playGame' `select` test') >> endpoints
  where
    start' = endpoint @"start" start
    move' = endpoint @"move" mkMove
    move2' = endpoint @"move2" mkMoveV2
    claimFirst' = endpoint @"claimFirst" claimFirst 
    claimSnd' = endpoint @"claimSnd" claimSnd     
    claimTieSnd' = endpoint @"claimTieSnd" claimTieSnd             
    moveS' = endpoint @"moveS" mkMoveS
    playGame' = endpoint @"playGame" playGame
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
-- Passing Scenario
myTracePlay :: Trace.EmulatorTrace ()
myTracePlay = do
    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints 
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints 
    Trace.callEndpoint @"test" h1 ()
    -- s <- Trace.waitNSlots 1
    void $ Trace.waitNSlots 1
    -- Second player has not joined the game yet.
    -- 1
    Trace.callEndpoint @"start" h1 $ StartParams
      {
        gFirstPlayerPkh = mockWalletPaymentPubKeyHash (knownWallet 1)
       ,gFirstPlayerMarker = (BuiltinByteString Haskell.. C.pack) "X"
       ,gSecondPlayerPkh = Nothing -- mockWalletPaymentPubKeyHash (knownWallet 2)
       ,gSecondPlayerMarker = Nothing -- (BuiltinByteString Haskell.. C.pack) "O"
       ,gNextMove = O
       ,gMinGameStake = minGameStake
       ,gStake = 10_000_000
       ,gDeadline = slotToBeginPOSIXTime def 20
       ,gCurrency = currSymbol
       ,gTokenName = tName
       ,gState = initGameState
      }
    -- void $ Trace.waitNSlots 1    
    s <- Trace.waitNSlots 1
    Extras.logInfo $ "reached " ++ Haskell.show s

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move second player with stake
    -- 2
    Trace.callEndpoint @"playGame" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p33") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move first player with stake
    -- 3
    Trace.callEndpoint @"playGame" h1 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p11") X 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move second player with stake
    -- 4
    Trace.callEndpoint @"playGame" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p31") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move first player with stake
    -- 5
    Trace.callEndpoint @"playGame" h1 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p12") X 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move second player with stake
    -- 6
    Trace.callEndpoint @"playGame" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p32") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1    
    -- void $ Trace.waitNSlots 30 -- Test claiming mechanism after deadline

    -- Uncomment to see the script failur when a player is still trying to make a move after the game has been won.
    -- -- Make a move first player with stake
    -- -- 7
    -- Trace.callEndpoint @"playGame" h1 $ MoveParams
    --   {
    --      mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
    --     ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
    --     ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
    --     ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
    --     ,mNextMove            = O        
    --     ,mStake               = 10_000_000
    --     ,mCurrency            = currSymbol
    --     ,mTokenName           = tName
    --     ,mState               = initGameState
    --     ,mMinGameStake        = minGameStake 
    --     ,mChoice              = Play (fromJust $ convLocations2Num "p13") X 
    --   }
    -- void $ Trace.waitNSlots 1

    -- Trace.callEndpoint @"test" h1 ()
    -- void $ Trace.waitNSlots 1

    -- Make a move first player with stake
    -- 7
    Trace.callEndpoint @"claimSnd" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = ClaimSecond -- Try to claim the prize
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1


myTraceTie :: Trace.EmulatorTrace ()
myTraceTie = do
    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints 
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints 
    Trace.callEndpoint @"test" h1 ()
    -- s <- Trace.waitNSlots 1
    void $ Trace.waitNSlots 1
    -- Second player has not joined the game yet.
    -- 1
    Trace.callEndpoint @"start" h1 $ StartParams
      {
        gFirstPlayerPkh = mockWalletPaymentPubKeyHash (knownWallet 1)
       ,gFirstPlayerMarker = (BuiltinByteString Haskell.. C.pack) "X"
       ,gSecondPlayerPkh = Nothing -- mockWalletPaymentPubKeyHash (knownWallet 2)
       ,gSecondPlayerMarker = Nothing -- (BuiltinByteString Haskell.. C.pack) "O"
       ,gNextMove = O
       ,gMinGameStake = minGameStake
       ,gStake = 10_000_000
       ,gDeadline = slotToBeginPOSIXTime def 40
       ,gCurrency = currSymbol
       ,gTokenName = tName
       ,gState = initGameState
      }
    -- void $ Trace.waitNSlots 1    
    s <- Trace.waitNSlots 1
    Extras.logInfo $ "reached " ++ Haskell.show s

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move second player with stake
    -- 2
    Trace.callEndpoint @"playGame" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p31") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move first player with stake
    -- 3
    Trace.callEndpoint @"playGame" h1 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p12") X 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move second player with stake
    -- 4
    Trace.callEndpoint @"playGame" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p32") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move first player with stake
    -- 5
    Trace.callEndpoint @"playGame" h1 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p33") X 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move second player with stake
    -- 6
    Trace.callEndpoint @"playGame" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p11") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move first player with stake
    -- 7
    Trace.callEndpoint @"playGame" h1 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p21") X 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1


    -- Make a move second player with stake
    -- 8
    Trace.callEndpoint @"playGame" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p13") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move first player with stake
    -- 9
    Trace.callEndpoint @"playGame" h1 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p22") X 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move second player with stake
    -- 10
    Trace.callEndpoint @"playGame" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p23") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1
    -- void $ Trace.waitNSlots 45 -- Try to claim after the deadline

    -- Uncomment to see the script failur when a player is still trying to make a move after the game has been won.
    -- -- Make a move first player with stake
    -- -- 7
    -- Trace.callEndpoint @"playGame" h1 $ MoveParams
    --   {
    --      mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
    --     ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
    --     ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
    --     ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
    --     ,mNextMove            = O        
    --     ,mStake               = 10_000_000
    --     ,mCurrency            = currSymbol
    --     ,mTokenName           = tName
    --     ,mState               = initGameState
    --     ,mMinGameStake        = minGameStake 
    --     ,mChoice              = Play (fromJust $ convLocations2Num "p13") X 
    --   }
    -- void $ Trace.waitNSlots 1

    -- Trace.callEndpoint @"test" h1 ()
    -- void $ Trace.waitNSlots 1

    -- Make a move first player with stake
    -- 7
    Trace.callEndpoint @"claimTieSnd" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = ClaimSecond -- Try to claim the prize
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1



-- test consuming the bounty when the deadline has not been reached
-- Passing Scenario
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
       ,gDeadline = slotToBeginPOSIXTime def 20
       ,gCurrency = currSymbol
       ,gTokenName = tName
       ,gState = initGameState
      }
    -- void $ Trace.waitNSlots 1    
    s <- Trace.waitNSlots 1
    Extras.logInfo $ "reached " ++ Haskell.show s

    Trace.callEndpoint @"test" h1 ()
    void $ Trace.waitNSlots 1

    -- Make a move second player with stake
    Trace.callEndpoint @"playGame" h2 $ MoveParams -- Use playGame endpoint instead of move2
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p11") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"move2" h1 $ MoveParams -- This will fail since the game state is not getting changed in this endpoint
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Nothing
        ,mSecondPlayerMarker  = Nothing
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p12") X
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h2 ()
    void $ Trace.waitNSlots 1

-- Failing Scenario
myTraceTestSteal :: Trace.EmulatorTrace ()
myTraceTestSteal = do
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

    -- Make a move second player with stake
    Trace.callEndpoint @"move2" h2 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Just $ mockWalletPaymentPubKeyHash (knownWallet 2)
        ,mSecondPlayerMarker  = Just $ (BuiltinByteString Haskell.. C.pack) "O"
        ,mNextMove            = X        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p11") O 
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"moveS" h1 $ MoveParams
      {
         mFirstPlayerPkh      = mockWalletPaymentPubKeyHash (knownWallet 1)
        ,mFirstPlayerMarker   = (BuiltinByteString Haskell.. C.pack) "X"
        ,mSecondPlayerPkh     = Nothing
        ,mSecondPlayerMarker  = Nothing
        ,mNextMove            = O        
        ,mStake               = 10_000_000
        ,mCurrency            = currSymbol
        ,mTokenName           = tName
        ,mState               = initGameState
        ,mMinGameStake        = minGameStake 
        ,mChoice              = Play (fromJust $ convLocations2Num "p12") X
      }
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"test" h2 ()
    void $ Trace.waitNSlots 1



-- ---------->
-- R1  R2  R3
-- 555_555_555

convLocations2Num :: Haskell.String -> Maybe Integer
convLocations2Num loc = Map.lookup loc mapLoc
    where mapLoc = Map.fromList[ ("p11",9),
                    ("p12",8), 
                    ("p13",7),
                    ("p21",6), 
                    ("p22",5),
                    ("p23",4), 
                    ("p31",3),
                    ("p32",2), 
                    ("p33",1)] 

currSymbol :: CurrencySymbol
currSymbol = currencySymbol "dcddcaa"

tName :: TokenName
tName = tokenName "T1"

tName2 :: TokenName
tName2 = tokenName "T2"

-- Create a token of AssetClass to send it to the wallet
token :: AssetClass
token = AssetClass (currSymbol, tName)

--Passing test
test1 :: IO ()
test1 = Trace.runEmulatorTraceIO' traceConfig emulatorConfig myTraceTest

-- Failing test
testS :: IO ()
testS = Trace.runEmulatorTraceIO' traceConfig emulatorConfig myTraceTestSteal

-- Play test
testPlay :: IO ()
testPlay = Trace.runEmulatorTraceIO' traceConfig emulatorConfig myTracePlay

-- Play Tie test
testTie :: IO ()
testTie = Trace.runEmulatorTraceIO' traceConfig emulatorConfig myTraceTie


-- test1 = Trace.runEmulatorTraceIO' def def myTraceTest