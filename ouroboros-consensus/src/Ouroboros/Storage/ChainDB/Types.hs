{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Ouroboros.Storage.ChainDB.Types
    ( ChainDBError(..)
    , prettyChainDBError
    ) where

import           Codec.Serialise (DeserialiseFailure)
import           Control.Exception (displayException)

import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, prettyCallStack)

import           Ouroboros.Network.Block (Slot, StandardHash)
import           Ouroboros.Network.Chain (Point)

import           Ouroboros.Storage.ImmutableDB.Types (ImmutableDBError,
                     prettyImmutableDBError, sameImmutableDBError)
import           Ouroboros.Storage.VolatileDB.Types (VolatileDBError,
                     sameVolatileDBError)

data ChainDBError block
  = ImmutableDBError    ImmutableDBError
  | VolatileDBError     (VolatileDBError (Point block))
  | DeserialiseError    DeserialiseFailure          CallStack
  | ReadFutureSlotError Slot Slot                   CallStack
    -- ^ The requested slot (first argument) was in the future, i.e. greater
    -- than the slot of the tip of the current chain (second argument).
  | MissingBlockError   (Either (Point block) Slot) CallStack
    -- ^ The block is not present in the 'ChainDB'.
  deriving (Show, Generic)

prettyChainDBError :: StandardHash block => ChainDBError block -> String
prettyChainDBError = \case
    ImmutableDBError  ie         -> prettyImmutableDBError ie
    VolatileDBError   ve         -> show                   ve
    DeserialiseError  df cs      ->
      displayException df <> ": " <> prettyCallStack cs
    ReadFutureSlotError rs ts cs ->
      "ReadFutureSlotError (requested was " <> show rs <>
      ", the slot of the tip is " <> show ts <> "): " <> prettyCallStack cs
    MissingBlockError ps cs      ->
      "MissingBlockError (" <> which  <> "): " <> prettyCallStack cs
      where
        which = either show show ps

instance StandardHash block => Eq (ChainDBError block) where
  ImmutableDBError  ie1         == ImmutableDBError  ie2         = sameImmutableDBError ie1 ie2
  ImmutableDBError  {}          == _                             = False
  VolatileDBError   ve1         == VolatileDBError   ve2         = sameVolatileDBError  ve1 ve2
  VolatileDBError   {}          == _                             = False
  DeserialiseError  df1 _       == DeserialiseError  df2 _       = df1 == df2
  DeserialiseError  {}          == _                             = False
  ReadFutureSlotError rs1 ts1 _ == ReadFutureSlotError rs2 ts2 _ = rs1 == rs2 && ts1 == ts2
  ReadFutureSlotError {}        == _                             = False
  MissingBlockError ps1 _       == MissingBlockError ps2 _       = ps1 == ps2
  MissingBlockError {}          == _                             = False
