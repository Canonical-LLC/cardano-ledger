{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : EpochBoundary
-- Description : Functions and definitions for rules at epoch boundary.
--
-- This modules implements the necessary functions for the changes that can happen at epoch boundaries.
module Cardano.Ledger.Shelley.EpochBoundary
  ( Stake (..),
    SnapShot (..),
    SnapShots (..),
    emptySnapShot,
    emptySnapShots,
    poolStake,
    obligation,
    maxPool,
    maxPool',

    -- * Deprecated
    BlocksMade,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Ledger.BaseTypes (BoundedRational (..), NonNegativeInterval)
import qualified Cardano.Ledger.BaseTypes as Core (BlocksMade)
import Cardano.Ledger.Coin
  ( Coin (..),
    coinToRational,
    rationalToCoinViaFloor,
  )
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Cardano.Ledger.Shelley.TxBody (PoolParams)
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData)
import Control.SetAlgebra (dom, eval, setSingleton, (▷), (◁))
import Data.Default.Class (Default, def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- | Type of stake as map from hash key to coins associated.
newtype Stake crypto = Stake
  {unStake :: Map (Credential 'Staking crypto) Coin}
  deriving (Show, Eq, Ord, NoThunks, NFData)

deriving newtype instance CC.Crypto crypto => ToCBOR (Stake crypto)

deriving newtype instance CC.Crypto crypto => FromCBOR (Stake crypto)

-- | Get stake of one pool
poolStake ::
  KeyHash 'StakePool crypto ->
  Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto) ->
  Stake crypto ->
  Stake crypto
poolStake hk delegs (Stake stake) =
  Stake (eval (dom (delegs ▷ setSingleton hk) ◁ stake))

-- | Calculate total possible refunds.
obligation ::
  forall crypto pp.
  (HasField "_keyDeposit" pp Coin, HasField "_poolDeposit" pp Coin) =>
  pp ->
  Map (Credential 'Staking crypto) Coin ->
  Map (KeyHash 'StakePool crypto) (PoolParams crypto) ->
  Coin
obligation pp rewards stakePools =
  (length rewards <×> getField @"_keyDeposit" pp)
    <+> (length stakePools <×> getField @"_poolDeposit" pp)

-- | Calculate maximal pool reward
maxPool' ::
  NonNegativeInterval ->
  Natural ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool' a0 nOpt r sigma pR = rationalToCoinViaFloor $ factor1 * factor2
  where
    z0 = 1 % fromIntegral nOpt
    sigma' = min sigma z0
    p' = min pR z0
    factor1 = coinToRational r / (1 + unboundRational a0)
    factor2 = sigma' + p' * unboundRational a0 * factor3
    factor3 = (sigma' - p' * factor4) / z0
    factor4 = (z0 - sigma') / z0

-- | Version of maxPool' that extracts a0 and nOpt from a PParam with the right HasField instances
maxPool ::
  (HasField "_a0" pp NonNegativeInterval, HasField "_nOpt" pp Natural) =>
  pp ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool pc r sigma pR = maxPool' a0 nOpt r sigma pR
  where
    a0 = getField @"_a0" pc
    nOpt = getField @"_nOpt" pc

-- | Snapshot of the stake distribution.
data SnapShot crypto = SnapShot
  { _stake :: !(Stake crypto),
    _delegations :: !(Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto)),
    _poolParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto))
  }
  deriving (Show, Eq, Generic)

instance NoThunks (SnapShot crypto)

instance NFData (SnapShot crypto)

instance
  CC.Crypto crypto =>
  ToCBOR (SnapShot crypto)
  where
  toCBOR
    SnapShot
      { _stake = s,
        _delegations = d,
        _poolParams = p
      } =
      encodeListLen 3
        <> toCBOR s
        <> toCBOR d
        <> toCBOR p

instance CC.Crypto crypto => FromCBOR (SnapShot crypto) where
  fromCBOR =
    decodeRecordNamed "SnapShot" (const 3) $
      SnapShot <$> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Snapshots of the stake distribution.
data SnapShots crypto = SnapShots
  { _pstakeMark :: SnapShot crypto,
    _pstakeSet :: !(SnapShot crypto),
    _pstakeGo :: !(SnapShot crypto),
    _feeSS :: !Coin
  }
  deriving (Show, Eq, Generic)

instance NoThunks (SnapShots crypto)

instance NFData (SnapShots crypto)

instance
  CC.Crypto crypto =>
  ToCBOR (SnapShots crypto)
  where
  toCBOR (SnapShots mark set go fs) =
    encodeListLen 4
      <> toCBOR mark
      <> toCBOR set
      <> toCBOR go
      <> toCBOR fs

instance
  CC.Crypto crypto =>
  FromCBOR (SnapShots crypto)
  where
  fromCBOR =
    decodeRecordNamed "SnapShots" (const 4) $
      SnapShots
        <$> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR

instance Default (SnapShots crypto) where
  def = emptySnapShots

emptySnapShot :: SnapShot crypto
emptySnapShot = SnapShot (Stake Map.empty) Map.empty Map.empty

emptySnapShots :: SnapShots crypto
emptySnapShots = SnapShots emptySnapShot emptySnapShot emptySnapShot (Coin 0)

{-# DEPRECATED BlocksMade "Import from Cardano.Ledger.BaseTypes instead" #-}

type BlocksMade = Core.BlocksMade
