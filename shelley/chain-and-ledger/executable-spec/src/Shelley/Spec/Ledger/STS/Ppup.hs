{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Ppup
  ( PPUP
  , PPUPEnv(..)
  )
where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLen, decodeWord,
                     encodeListLen, matchSize)
import           Cardano.Ledger.Shelley.Crypto (Crypto)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Ledger.Core (dom, (⊆), (⨃))
import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.Keys
import           Shelley.Spec.Ledger.PParams
import           Shelley.Spec.Ledger.Slot
import           Shelley.Spec.Ledger.Updates

data PPUP crypto

data PPUPEnv crypto
  = PPUPEnv SlotNo PParams (GenDelegs crypto)

instance STS (PPUP crypto) where
  type State (PPUP crypto) = PPUpdate crypto
  type Signal (PPUP crypto) = Maybe (Update crypto)
  type Environment (PPUP crypto) = PPUPEnv crypto
  type BaseM (PPUP crypto) = ShelleyBase
  data PredicateFailure (PPUP crypto)
    = NonGenesisUpdatePPUP (Set (GenKeyHash crypto)) (Set (GenKeyHash crypto))
    | PPUpdateTooLatePPUP
    | PPUpdateWrongEpoch EpochNo
    | PVCannotFollowPPUP
    deriving (Show, Eq, Generic)

  initialRules = []

  transitionRules = [ ppupTransitionNonEmpty ]

instance NoUnexpectedThunks (PredicateFailure (PPUP crypto))

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (PredicateFailure (PPUP crypto))
 where
   toCBOR = \case
     (NonGenesisUpdatePPUP a b) ->
       encodeListLen 3
       <> toCBOR (0 :: Word8)
       <> toCBOR a
       <> toCBOR b
     PPUpdateTooLatePPUP    -> encodeListLen 1 <> toCBOR (1 :: Word8)
     (PPUpdateWrongEpoch e) -> encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR e
     PVCannotFollowPPUP     -> encodeListLen 1 <> toCBOR (3 :: Word8)

instance
  (Crypto crypto)
  => FromCBOR (PredicateFailure (PPUP crypto))
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "NonGenesisUpdatePPUP" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ NonGenesisUpdatePPUP a b
      1 -> matchSize "PPUpdateTooLatePPUP" 1 n >> pure PPUpdateTooLatePPUP
      2 -> do
        matchSize "PPUpdateWrongEpoch" 2 n
        a <- fromCBOR
        pure $ PPUpdateWrongEpoch a
      3 -> matchSize "PVCannotFollowPPUP" 1 n >> pure PVCannotFollowPPUP
      k -> invalidKey k

pvCanFollow :: ProtVer -> Ppm -> Bool
pvCanFollow (ProtVer m n) (ProtocolVersion (ProtVer m' n'))
  = (m+1, 0) == (m', n') || (m, n+1) == (m', n')
pvCanFollow _ _ = True

ppupTransitionNonEmpty :: TransitionRule (PPUP crypto)
ppupTransitionNonEmpty = do
  TRC (PPUPEnv slot pp (GenDelegs _genDelegs), PPUpdate pupS, up)
    <- judgmentContext

  case up of
    Nothing -> pure (PPUpdate pupS)
    Just (Update (PPUpdate pup) te) -> do

      (dom pup ⊆ dom _genDelegs) ?! NonGenesisUpdatePPUP (dom pup) (dom _genDelegs)

      all (all (pvCanFollow (_protocolVersion pp)) . ppmSet) pup ?! PVCannotFollowPPUP

      sp <- liftSTS $ asks slotsPrior
      firstSlotNextEpoch <- liftSTS $ do
        ei <- asks epochInfo
        EpochNo e <- epochInfoEpoch ei slot
        epochInfoFirst ei (EpochNo $ e + 1)
      slot < firstSlotNextEpoch *- (Duration (2 * sp)) ?! PPUpdateTooLatePPUP

      currentEpoch <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoEpoch ei slot
      currentEpoch == te ?! PPUpdateWrongEpoch te

      pure $ PPUpdate (pupS ⨃  Map.toList pup)
