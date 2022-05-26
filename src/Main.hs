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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- A game with two players. Player 1 thinks of a secret word
-- and uses its hash, and the game validator script, to lock
-- some funds (the prize) in a pay-to-script transaction output.
-- Player 2 guesses the word by attempting to spend the transaction
-- output. If the guess is correct, the validator script releases the funds.
-- If it isn't, the funds stay locked.
module Main where

import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Ledger (Address, dataHash ,Datum (Datum), DatumHash (..), ScriptContext, Validator, Value, getCardanoTxId)
import Ledger 
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


data Game = Game
    { tFirstPlayer        :: !PaymentPubKeyHash
    , tSecondPlayer       :: !PaymentPubKeyHash
    , tGameStake          :: !Integer
    , tDeadline           :: !POSIXTime
    , identifierToken     :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''Game

data GameChoice = X | O
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    X == X = True
    O == O = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''GameChoice

data GameDatum = GameDatum (Map [Haskell.Char] [Haskell.Char])
    deriving Show