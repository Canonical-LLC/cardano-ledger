module Cardano.Rewards.Common
  ( poolR
  , memberRew
  , leaderRew
  , poolRewardInfo
  , getRewardParameters
  ) where

import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Set as Set

import Cardano.Rewards.Types

maxPool ::
  ProtocolParameters ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool pp r sigma pR = coinFloor $ factor1 * factor2
  where
    z0 = 1 % κ pp
    sigma' = min sigma z0
    p' = min pR z0
    factor1 :: Rational
    factor1 = coinToQ r / (1 + asc pp)
    factor2 = sigma' + p' * asc pp * factor3
    factor3 = (sigma' - p' * factor4) / z0
    factor4 = (z0 - sigma') / z0

mkApparentPerformance ::
  ProtocolParameters ->
  Rational ->
  BlockCount ->
  BlockCount ->
  Rational
mkApparentPerformance pp sigma (BlockCount blocksN) (BlockCount blocksTotal)
  | sigma == 0 = 0
  | d pp < 0.8 = beta / sigma
  | otherwise = 1
  where
    beta = blocksN % max 1 blocksTotal

maxP ::
  ProtocolParameters ->
  PoolParameters c ->
  Coin ->
  Coin ->
  Rational ->
  Coin ->
  Coin
maxP pp pool r totalStake sigma ownerLovelace =
  if pledge pool <= ownerLovelace
    then maxPool pp r sigma pledgeRatio
    else 0
  where
    pledgeRatio = coinRatio (pledge pool) totalStake
poolR ::
  ProtocolParameters ->
  PoolParameters c ->
  Rational ->
  BlockCount ->
  BlockCount ->
  Coin ->
  Coin ->
  Rational ->
  Coin ->
  Coin
poolR pp pool sigmaA blocksN blocksTotal totalStake r sigma ownerLovelace =
  coinFloor (appPerf * coinToQ mp)
  where
    appPerf = mkApparentPerformance pp sigmaA blocksN blocksTotal
    mp = maxP pp pool r totalStake sigma ownerLovelace

-- | Calculate pool leader reward
leaderRew ::
  Coin ->
  PoolParameters c ->
  Rational ->
  Rational ->
  Coin
leaderRew f pool s sigma
  | f <= c = f
  | otherwise =
    c <> coinFloor (coinToQ (f - c) * (m + (1 - m) * s / sigma))
  where
    c = cost pool
    m = margin pool

-- | Calculate pool member reward
memberRew ::
  Coin ->
  PoolParameters c ->
  Rational ->
  Rational ->
  Coin
memberRew f pool t sigma
  | f <= c = mempty
  | otherwise =
    coinFloor $ coinToQ (f - c) * (1 - m) * t / sigma
  where
    c = cost pool
    m = margin pool

-- | Get stake of one pool
poolStake ::
  (Eq p, Ord c) =>
  PoolID p ->
  Delgations p c ->
  StakeDistribution c ->
  StakeDistribution c
poolStake poolID delegations =
  Map.filterWithKey f
  where
    f cred _ =
      case Map.lookup cred delegations of
        Nothing -> False
        Just p -> p == poolID

poolRewardInfo ::
  (Ord p, Ord c) =>
  ProtocolParameters ->
  BlocksMade p ->
  BlockCount ->
  StakeDistribution c ->
  Delgations p c ->
  Coin ->
  Coin ->
  Coin ->
  PoolID p ->
  PoolParameters c ->
  PoolRewardInfo c
poolRewardInfo pp blocks blocksTotal sd delegations totalStake total_a r pid pool =
  PoolRewardInfo { pooledStake = pstake
                 , poolRelativeStake = sigma'
                 , ownerStake = ostk
                 , poolPot = poolR'
                 , poolPs = pool
                 }
  where
    pstake = poolStake pid delegations sd
    pstakeTot = sum pstake
    ostk = Set.foldl' (\c o -> c + Map.findWithDefault 0 o pstake) 0 (owners pool)
    blocksN = Map.findWithDefault 0 pid blocks
    sigma' = coinRatio pstakeTot totalStake
    sigmaA' = coinRatio pstakeTot total_a
    poolR' = poolR pp pool sigmaA' blocksN blocksTotal totalStake r sigma' ostk

getRewardParameters ::
  ProtocolParameters ->
  BlocksMade p ->
  Coin ->
  Coin ->
  Coin ->
  Integer ->
  (Coin, Coin, BlockCount)
getRewardParameters pp blocks fees reserves maxSupply slotsPerEpoch =
  (_R, totalStake, blocksTotal)
  where
    blocksTotal = sum blocks
    expectedBlocks = floor $ (1 - d pp) * asc pp * fromIntegral slotsPerEpoch
    η
      | d pp >= 0.8 = 1
      | otherwise = unBlockCount blocksTotal % expectedBlocks
    δr = coinFloor $ min 1 η * ρ pp * coinToQ reserves
    rewardPot = fees + δr
    δt = coinFloor $ τ pp * coinToQ rewardPot
    _R = rewardPot - δt
    totalStake = maxSupply - reserves
