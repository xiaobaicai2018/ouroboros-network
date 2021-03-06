{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Protocol.Praos (
    StakeDist
  , Praos
  , PraosExtraFields(..)
  , PraosParams(..)
    -- * Tags
  , PraosCrypto(..)
  , PraosStandardCrypto
  , PraosMockCrypto
    -- * Type instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           Codec.Serialise (Serialise, encode)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Numeric.Natural

import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import qualified Ouroboros.Network.Chain as Chain

import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.Hash.Class (HashAlgorithm (..),
                     fromHash, hash)
import           Ouroboros.Consensus.Crypto.Hash.MD5 (MD5)
import           Ouroboros.Consensus.Crypto.Hash.SHA256 (SHA256)
import           Ouroboros.Consensus.Crypto.KES.Class
import           Ouroboros.Consensus.Crypto.KES.Mock
import           Ouroboros.Consensus.Crypto.KES.Simple
import           Ouroboros.Consensus.Crypto.VRF.Class
import           Ouroboros.Consensus.Crypto.VRF.Mock (MockVRF)
import           Ouroboros.Consensus.Crypto.VRF.Simple (SimpleVRF)
import           Ouroboros.Consensus.Node (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Util.Chain (forksAtMostKBlocks, upToSlot)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.HList (HList)
import           Ouroboros.Consensus.Util.HList (HList (..))

{-------------------------------------------------------------------------------
  Praos specific types
-------------------------------------------------------------------------------}

data VRFType = NONCE | TEST
    deriving (Show, Eq, Ord, Generic)

instance Serialise VRFType
  -- use generic instance

data PraosExtraFields c = PraosExtraFields {
      praosCreator :: CoreNodeId
    , praosRho     :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    , praosY       :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    }
  deriving Generic

deriving instance PraosCrypto c => Show (PraosExtraFields c)
deriving instance PraosCrypto c => Eq   (PraosExtraFields c)
deriving instance PraosCrypto c => Ord  (PraosExtraFields c)

instance VRFAlgorithm (PraosVRF c) => Serialise (PraosExtraFields c)
  -- use Generic instance for now

data PraosProof c = PraosProof {
      praosProofRho  :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    , praosProofY    :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    , praosLeader    :: CoreNodeId
    , praosProofSlot :: SlotNo
    }

data PraosValidationError c =
      PraosInvalidSlot SlotNo SlotNo
    | PraosUnknownCoreId Int
    | PraosInvalidSig (VerKeyKES (PraosKES c)) Natural (SigKES (PraosKES c))
    | PraosInvalidCert (VerKeyVRF (PraosVRF c)) Encoding Natural (CertVRF (PraosVRF c))
    | PraosInsufficientStake Double Natural

deriving instance PraosCrypto c => Show (PraosValidationError c)

-- TODO: This type definition belongs elsewhere.
newtype EpochNo = EpochNo { unEpochNo :: Word64 }
    deriving (Eq, Num, Ord, Serialise)

type StakeDist = IntMap Rational

data BlockInfo c = BlockInfo
    { biSlot  :: SlotNo
    , biRho   :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    , biStake :: StakeDist
    }

deriving instance PraosCrypto c => Show (BlockInfo c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data Praos c

-- | Praos parameters that are node independent
data PraosParams = PraosParams {
      praosLeaderF       :: Double
    , praosSecurityParam :: SecurityParam
    , praosSlotsPerEpoch :: Word64
    , praosLifetimeKES   :: Natural
    }

instance PraosCrypto c => OuroborosTag (Praos c) where

  data Payload (Praos c) ph = PraosPayload {
        praosSignature   :: SignedKES (PraosKES c) (ph, PraosExtraFields c)
      , praosExtraFields :: PraosExtraFields c
      }
    deriving (Generic)

  data NodeConfig (Praos c) = PraosNodeConfig
    { praosParams        :: PraosParams
    , praosInitialEta    :: Natural
    , praosInitialStake  :: StakeDist
    , praosNodeId        :: NodeId
    , praosSignKeyVRF    :: SignKeyVRF (PraosVRF c)
    , praosVerKeys       :: IntMap (VerKeyKES (PraosKES c), VerKeyVRF (PraosVRF c))
    }

  protocolSecurityParam = praosSecurityParam . praosParams

  type NodeState      (Praos c) = SignKeyKES (PraosKES c)
  type LedgerView     (Praos c) = StakeDist
  type IsLeader       (Praos c) = PraosProof c
  type ValidationErr  (Praos c) = PraosValidationError c
  type SupportedBlock (Praos c) = HasPayload (Praos c)
  type ChainState     (Praos c) = [BlockInfo c]

  mkPayload PraosNodeConfig{..} PraosProof{..} preheader = do
      keyKES <- getNodeState
      let extraFields = PraosExtraFields {
            praosCreator = praosLeader
          , praosRho     = praosProofRho
          , praosY       = praosProofY
          }
      m <- signedKES
        (fromIntegral (unSlotNo praosProofSlot))
        (preheader, extraFields)
        keyKES
      case m of
        Nothing               -> error "mkOutoborosPayload: signedKES failed"
        Just (signed, newKey) -> do
          putNodeState newKey
          return $ PraosPayload {
              praosSignature   = signed
            , praosExtraFields = extraFields
            }

  checkIsLeader cfg@PraosNodeConfig{..} slot _u cs =
    case praosNodeId of
        RelayId _  -> return Nothing
        CoreId nid -> do
          let (rho', y', t) = rhoYT cfg cs slot nid
          rho <- evalCertified rho' praosSignKeyVRF
          y   <- evalCertified y'   praosSignKeyVRF
          return $ if fromIntegral (certifiedNatural y) < t
              then Just PraosProof {
                       praosProofRho  = rho
                     , praosProofY    = y
                     , praosLeader    = CoreNodeId nid
                     , praosProofSlot = slot
                     }
              else Nothing

  applyChainState cfg@PraosNodeConfig{..} sd b cs = do
    let PraosPayload{..} = blockPayload (Proxy :: Proxy (Praos c)) b
        ph               = blockPreHeader b
        slot             = blockSlot b
        CoreNodeId nid   = praosCreator praosExtraFields

    -- check that the new block advances time
    case cs of
        (c : _)
            | biSlot c >= slot -> throwError $ PraosInvalidSlot slot (biSlot c)
        _                      -> return ()

    -- check that block creator is a known core node
    (vkKES, vkVRF) <- case IntMap.lookup nid praosVerKeys of
        Nothing  -> throwError $ PraosUnknownCoreId nid
        Just vks -> return vks

    -- verify block signature
    unless (verifySignedKES
                vkKES
                (fromIntegral $ unSlotNo slot)
                (ph, praosExtraFields)
                praosSignature) $
        throwError $ PraosInvalidSig vkKES (fromIntegral $ unSlotNo slot) (getSig praosSignature)

    let (rho', y', t) = rhoYT cfg cs slot nid
        rho           = praosRho praosExtraFields
        y             = praosY   praosExtraFields

    -- verify rho proof
    unless (verifyCertified vkVRF rho' rho) $
        throwError $ PraosInvalidCert
            vkVRF
            (encode rho')
            (certifiedNatural rho)
            (certifiedProof rho)

    -- verify y proof
    unless (verifyCertified vkVRF y' y) $
        throwError $ PraosInvalidCert
            vkVRF
            (encode y')
            (certifiedNatural y)
            (certifiedProof y)

    -- verify stake
    unless (fromIntegral (certifiedNatural y) < t) $
        throwError $ PraosInsufficientStake t $ certifiedNatural y

    let bi = BlockInfo
            { biSlot  = blockSlot b
            , biRho   = praosRho praosExtraFields
            , biStake = sd
            }

    return $ bi : cs

  -- NOTE: We redefine `preferCandidate` but NOT `compareCandidates`
  -- NOTE: See note regarding clock skew.
  preferCandidate PraosNodeConfig{..} slot ours cand
    | forksAtMostKBlocks k ours' cand' &&
      Chain.length cand' > Chain.length ours'
    = Just cand'
    | otherwise
    = Nothing
    where
      clip = upToSlot (succ slot)

      ours' = clip ours
      cand' = clip cand

      PraosParams{..} = praosParams

      k :: Word64
      k = maxRollbacks praosSecurityParam


deriving instance PraosCrypto c => Show (Payload (Praos c) ph)
deriving instance PraosCrypto c => Eq   (Payload (Praos c) ph)
deriving instance PraosCrypto c => Ord  (Payload (Praos c) ph)

instance PraosCrypto c => Condense (Payload (Praos c) ph) where
    condense (PraosPayload sig _) = condense sig

instance (PraosCrypto c, Serialise ph) => Serialise (Payload (Praos c) ph) where
  -- use generic instance

slotEpoch :: NodeConfig (Praos c) -> SlotNo -> EpochNo
slotEpoch PraosNodeConfig{..} s =
    EpochNo $ 1 + div (unSlotNo s - 1) praosSlotsPerEpoch
  where
    PraosParams{..} = praosParams

blockInfoEpoch :: NodeConfig (Praos c) -> BlockInfo c -> EpochNo
blockInfoEpoch l = slotEpoch l . biSlot

epochStart :: NodeConfig (Praos c) -> EpochNo -> SlotNo
epochStart PraosNodeConfig{..} e = SlotNo $ (unEpochNo e - 1) * praosSlotsPerEpoch + 1
  where
    PraosParams{..} = praosParams

infosSlice :: SlotNo -> SlotNo -> [BlockInfo c] -> [BlockInfo c]
infosSlice from to xs = takeWhile (\b -> biSlot b >= from)
                      $ dropWhile (\b -> biSlot b > to) xs

infosEta :: forall c. PraosCrypto c
         => NodeConfig (Praos c)
         -> [BlockInfo c]
         -> EpochNo
         -> Natural
infosEta l _  1 = praosInitialEta l
infosEta l xs e =
    let e'   = e - 1
        eta' = infosEta l xs e'
        from = epochStart l e'
        n    = div (2 * praosSlotsPerEpoch) 3
        to   = SlotNo $ unSlotNo from + fromIntegral (n - 1)
        rhos = reverse [biRho b | b <- infosSlice from to xs]
    in  fromHash $ hash @(PraosHash c) $ eta' :* e :* rhos :* Nil
  where
    PraosParams{..} = praosParams l

infosStake :: NodeConfig (Praos c) -> [BlockInfo c] -> EpochNo -> StakeDist
infosStake s@PraosNodeConfig{..} xs e = case ys of
    []                  -> praosInitialStake
    (BlockInfo{..} : _) -> biStake
  where
    PraosParams{..} = praosParams

    e' = if e >= 2 then EpochNo (unEpochNo e - 2) else 0
    ys = dropWhile (\b -> blockInfoEpoch s b > e') xs

phi :: NodeConfig (Praos c) -> Rational -> Double
phi PraosNodeConfig{..} r = 1 - (1 - praosLeaderF) ** fromRational r
  where
    PraosParams{..} = praosParams

leaderThreshold :: forall c. PraosCrypto c
                => NodeConfig (Praos c)
                -> [BlockInfo c]
                -> SlotNo
                -> Int
                -> Double
leaderThreshold st xs s n =
    let a = IntMap.findWithDefault 0 n $ infosStake st xs (slotEpoch st s)
    in  2 ^ (byteCount (Proxy :: Proxy (PraosHash c)) * 8) * phi st a

rhoYT :: PraosCrypto c
      => NodeConfig (Praos c)
      -> [BlockInfo c]
      -> SlotNo
      -> Int
      -> ( HList '[Natural, SlotNo, VRFType]
         , HList '[Natural, SlotNo, VRFType]
         , Double
         )
rhoYT st xs s n =
    let e   = slotEpoch st s
        eta = infosEta st xs e
        rho = eta :* s :* NONCE :* Nil
        y   = eta :* s :* TEST  :* Nil
        t   = leaderThreshold st xs s n
    in  (rho, y, t)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

class ( KESAlgorithm  (PraosKES c)
      , VRFAlgorithm  (PraosVRF c)
      , HashAlgorithm (PraosHash c)
      ) => PraosCrypto (c :: *) where
  type family PraosKES c  :: *
  type family PraosVRF c  :: *
  type family PraosHash c :: *

data PraosStandardCrypto
data PraosMockCrypto

instance PraosCrypto PraosStandardCrypto where
  type PraosKES  PraosStandardCrypto = SimpleKES Ed448DSIGN
  type PraosVRF  PraosStandardCrypto = SimpleVRF
  type PraosHash PraosStandardCrypto = SHA256

instance PraosCrypto PraosMockCrypto where
  type PraosKES  PraosMockCrypto = MockKES
  type PraosVRF  PraosMockCrypto = MockVRF
  type PraosHash PraosMockCrypto = MD5
