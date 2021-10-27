module Cardano.Rewards.Original where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Cardano.Rewards.Common
import Cardano.Rewards.Types

rewardOnePool ::
  Ord c =>
  Coin ->
  PoolParameters c ->
  PoolRewardInfo c ->
  Rewards c
rewardOnePool
  totalStake
  pool
  (PoolRewardInfo stake sigma ownerLovelace poolRPot poolRI) =
    Map.insertWith (<>) (poolStakeCred pool) lReward mRewards
    where
      f c = memberRew poolRPot pool (coinRatio c totalStake) sigma
      mRewards = Map.map f (Map.filterWithKey notPoolOwner stake)
      notPoolOwner cred _ = cred `Set.notMember` owners poolRI
      lReward = leaderRew poolRPot pool (coinRatio ownerLovelace totalStake) sigma

rewards ::
  (Ord p, Ord c) =>
  ProtocolParameters ->
  Pools p c ->
  BlocksMade p ->
  Delgations p c ->
  StakeDistribution c ->
  Coin ->
  Coin ->
  Coin ->
  Integer ->
  Rewards c
rewards pp pools blocks delegations sd fees reserves maxSupply slotsPerEpoch =
  Map.foldlWithKey f mempty pools
  where
    (_R, totalStake, blocksTotal) = getRewardParameters pp blocks fees reserves maxSupply slotsPerEpoch
    totA = sum sd
    f acc pid pool =
      Map.unionWith (<>) acc $
        rewardOnePool
          totalStake
          pool
          (poolRewardInfo pp blocks blocksTotal sd delegations totalStake totA _R pid pool)
