{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}

module Ouroboros.Consensus.Ledger.Byron where

import Cardano.Chain.Block
import qualified Cardano.Chain.Genesis as Genesis
import           Control.Monad.Except
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Ouroboros.Consensus.Ledger.Abstract

-- | Newtype wrapper to avoid orphan instances
newtype ByronBlock = ByronBlock { unByronBlock :: ABlock ByteString }
  deriving (Eq, Show)

instance HasHeader ByronBlock where
  type HeaderHash b = Hash

instance UpdateLedger ByronBlock where
  newtype LedgerState ByronBlock = ByronLedgerState ChainValidationState
    deriving (Eq, Show)
  newtype LedgerError ByronBlock = ByronLedgerError ChainValidationError
    deriving (Eq, Show)
  newtype LedgerConfig ByronBlock = ByronLedgerConfig Genesis.Config

  applyLedgerBlock (ByronLedgerConfig cfg) (ByronBlock block) (ByronLedgerState state)
    = mapExcept (bimap ByronLedgerError ByronLedgerState) $ updateBody cfg state block

  applyLedgerHeader (ByronLedgerConfig cfg) (ByronBlock block) (ByronLedgerState state)
    = mapExcept (bimap ByronLedgerError ByronLedgerState) $ updateHeader cfg state block
