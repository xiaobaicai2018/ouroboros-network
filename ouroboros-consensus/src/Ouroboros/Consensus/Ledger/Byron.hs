{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}

module Ouroboros.Consensus.Ledger.Byron where

import           Cardano.Binary.Class (DecoderError (..))
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Crypto as Crypto
import Cardano.Prelude (cborError)
import           Control.Monad.Except
import qualified Crypto.Hash
import Data.Bifunctor (bimap)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import Data.Coerce
import Data.FingerTree (Measured(..))
import Data.Typeable (Typeable)
import Data.Word
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Network.Block
import Ouroboros.Network.Serialise

-- | Hard-coded number of slots per epoch in the Byron era
byronEpochSlots :: CC.Slot.EpochSlots
byronEpochSlots = CC.Slot.EpochSlots 21600

-- | Newtype wrapper to avoid orphan instances
newtype ByronBlock = ByronBlock { unByronBlock :: CC.Block.ABlock ByteString }
  deriving (Eq, Show)

instance StandardHash ByronBlock

-- Lifted from the similar `Bi` instance in cardano-sl-crypto.
instance (Typeable algo, Typeable a, Crypto.HashAlgorithm algo) => Serialise (Crypto.AbstractHash algo a) where
  encode (Crypto.AbstractHash digest) =
    encode (ByteArray.convert digest :: ByteString)

  -- FIXME bad decode: it reads an arbitrary-length byte string.
  -- Better instance: know the hash algorithm up front, read exactly that
  -- many bytes, fail otherwise. Then convert to a digest.
  decode = do
    bs <- decode @ByteString
    maybe
      (cborError $ DecoderErrorCustom
        "AbstractHash"
        "Cannot convert ByteString to digest"
      )
      (pure . Crypto.AbstractHash)
      (Crypto.Hash.digestFromByteString bs)

instance Measured BlockMeasure ByronBlock where
  measure = blockMeasure

instance HasHeader ByronBlock where
  type HeaderHash ByronBlock = CC.Block.HeaderHash

  blockHash = CC.Block.blockHash . fmap (const ()) . unByronBlock
  -- TODO distinguish the genesis hash? How do we do this after the fact?
  blockPrevHash = BlockHash . CC.Block.blockPrevHash . unByronBlock
  blockSlot = fromIntegral @Word64 . coerce . CC.Slot.flattenSlotId byronEpochSlots . CC.Block.blockSlot . unByronBlock
  blockNo = BlockNo . fromIntegral . CC.Block.blockDifficulty . unByronBlock
  blockInvariant = const True


instance UpdateLedger ByronBlock where
  newtype LedgerState ByronBlock = ByronLedgerState CC.Block.ChainValidationState
    deriving (Eq, Show)
  newtype LedgerError ByronBlock = ByronLedgerError CC.Block.ChainValidationError
    deriving (Eq, Show)
  newtype LedgerConfig ByronBlock = ByronLedgerConfig Genesis.Config

  applyLedgerBlock (ByronLedgerConfig cfg) (ByronBlock block) (ByronLedgerState state)
    = mapExcept (bimap ByronLedgerError ByronLedgerState) $ CC.Block.updateBody cfg state block

  applyLedgerHeader (ByronLedgerConfig cfg) (ByronBlock block) (ByronLedgerState state)
    = mapExcept (bimap ByronLedgerError ByronLedgerState) $ CC.Block.updateHeader cfg state block
