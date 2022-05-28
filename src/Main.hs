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

module Main where

import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
-- import Data.Map (Map)
-- import Data.Map qualified as Map
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
-- import PlutusTx.Builtins
import PlutusTx.AssocMap qualified as AssocMap

type AMap = AssocMap.Map 

data Game = Game
    { tFirstPlayer        :: !(BuiltinByteString, PaymentPubKeyHash)
    , tSecondPlayer       :: !(BuiltinByteString, PaymentPubKeyHash)
    , tGameStake          :: !Integer
    , tDeadline           :: !POSIXTime
    , identifierToken     :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''Game

data GameChoice = X | O | N
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    X == X = True
    O == O = True
    N == N = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''GameChoice

data GameDatum = GameDatum (AMap BuiltinByteString GameChoice) | NoDatum
    deriving (Show, Haskell.Eq)

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum cs == GameDatum cs' = (cs == cs')

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE initDatum #-}
initDatum :: GameDatum
initDatum = GameDatum (AssocMap.fromList t)
              where 
                  y = (\x -> (("p" Haskell.<> Haskell.show x) <> ) Haskell.<$> ((Haskell.show) Haskell.<$> [1..3::Integer] )) Haskell.<$> [1..3::Integer]
                  t = zip ((toBuiltin . C.pack) Haskell.<$> (concat y)) (Haskell.take 9 $ Haskell.repeat N)

{-# INLINABLE getDatumMap #-}
getDatumMap :: GameDatum -> (AMap BuiltinByteString GameChoice)
getDatumMap (GameDatum m) = m 

{-# INLINABLE mkValidator #-}
mkValidator :: Game -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkValidator game datum redeemer ctx = Haskell.undefined