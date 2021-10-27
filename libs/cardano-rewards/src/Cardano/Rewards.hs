module Cardano.Rewards where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
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
  Rewards c
rewards pp pools blocks delegations sd fees reserves =
  Map.foldlWithKey f mempty pools
  where
    (_R, totalStake, blocksTotal) = getRewardParameters pp blocks fees reserves
    totA = sum sd
    f acc pid pool =
      Map.unionWith (<>) acc $
        rewardOnePool
          totalStake
          pool
          (poolRewardInfo pp blocks blocksTotal sd delegations totalStake totA _R pid pool)

-- REWRITE --

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

rewards2 ::
  (Ord p, Ord c) =>
  ProtocolParameters ->
  Pools p c ->
  BlocksMade p ->
  Delgations p c ->
  StakeDistribution c ->
  Coin ->
  Coin ->
  Rewards c
rewards2 pp pools blocks delegations sd fees reserves = Map.unionWith (<>) mRs lRs
  where
    (r, totalStake, blocksTotal) = getRewardParameters pp blocks fees reserves
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

-- Example --
aliceSC :: StakeCredential String
aliceSC = StakeCredential "alice"

bobSC :: StakeCredential String
bobSC = StakeCredential "bob"

carlSC :: StakeCredential String
carlSC = StakeCredential "carl"

ppEx :: ProtocolParameters
ppEx = ProtocolParameters
       { asc = 1 % 20
       , d = 1 % 100
       , κ = 2
       , ρ = 1 % 100
       , τ = 20 % 100
       }

poolsEx :: Pools String String
poolsEx =
  Map.fromList
   [ (PoolID "alice", PoolParameters 100 (50%100) 100 (Set.singleton aliceSC) aliceSC)
   , (PoolID "bob", PoolParameters 1 (50%100) 1000 (Set.singleton bobSC) bobSC)
   ]

blocksEx :: BlocksMade String
blocksEx =
  Map.fromList
    [ (PoolID "alice", 4)
    , (PoolID "bob", 2)
    ]

delegationsEx :: Delgations String String
delegationsEx =
  Map.fromList
   [ (aliceSC, PoolID "alice")
   , (carlSC, PoolID "alice")
   , (bobSC, PoolID "bob")
   ]
  
stakeEx :: StakeDistribution String
stakeEx =
  Map.fromList
   [ (aliceSC, 10000000)
   , (carlSC, 10000000)
   , (bobSC, 10000000)
   ]
  
feesEx :: Coin
feesEx = 100

reservesEx :: Coin
reservesEx = 1000000

example :: Rewards String
example = rewards ppEx poolsEx blocksEx delegationsEx stakeEx feesEx reservesEx

example2 :: Rewards String
example2 = rewards2 ppEx poolsEx blocksEx delegationsEx stakeEx feesEx reservesEx
