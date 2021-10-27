module Cardano.Rewards.Refined where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Cardano.Rewards.Common
import Cardano.Rewards.Types

getPoolRewardParameters ::
  (Ord p, Ord c) =>
  ProtocolParameters ->
  Pools p c ->
  BlocksMade p ->
  BlockCount ->
  Delgations p c ->
  StakeDistribution c ->
  Coin ->
  Coin ->
  Map (PoolID p) (PoolRewardInfo c)
getPoolRewardParameters pp pools blocks blocksTotal delegations sd totalStake r =
  Map.mapWithKey f pools
  where
    f = poolRewardInfo pp blocks blocksTotal sd delegations totalStake totA r
    totA = sum sd

computeMemberRewards' ::
  (Ord p, Ord c) =>
  Coin ->
  Map (PoolID p) (PoolRewardInfo c) ->
  Delgations p c ->
  StakeCredential c ->
  Coin ->
  Maybe Coin
computeMemberRewards' totalStake poolRIs ds sc c = do
  pid <- Map.lookup sc ds
  poolRI <- Map.lookup pid poolRIs
  if sc `Set.member` (owners . poolPs $ poolRI)
    then Nothing
    else Just $ memberRew
           (poolPot poolRI)
           (poolPs poolRI)
           (coinRatio c totalStake)
           (poolRelativeStake poolRI)

computeMemberRewards ::
  (Ord p, Ord c) =>
  Coin ->
  Map (PoolID p) (PoolRewardInfo c) ->
  Delgations p c ->
  StakeCredential c ->
  Coin ->
  Coin
computeMemberRewards totalStake poolRIs ds sc c =
  fromMaybe 0 $ computeMemberRewards' totalStake poolRIs ds sc c

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
  Map.unionWith (<>) mRs lRs
  where
    (r, totalStake, blocksTotal) = getRewardParameters pp blocks fees reserves maxSupply slotsPerEpoch
    poolInfos = getPoolRewardParameters pp pools blocks blocksTotal delegations sd totalStake r
    mRs = Map.mapWithKey (computeMemberRewards totalStake poolInfos delegations) sd
    f acc poolRI =
      Map.insertWith
        (<>)
        (poolStakeCred . poolPs $ poolRI)
        (leaderRew
          (poolPot poolRI)
          (poolPs poolRI)
          (coinRatio (ownerStake poolRI) totalStake)
          (poolRelativeStake poolRI)
        )
        acc
    lRs = Map.foldl' f mempty poolInfos
