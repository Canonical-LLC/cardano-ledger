{-# LANGUAGE DerivingVia #-}

module Cardano.Rewards.Types
  ( Coin
  , coinFloor
  , coinToQ
  , coinRatio
  , BlockCount (..)
  , StakeCredential (..)
  , PoolID (..)
  , PoolParameters (..)
  , ProtocolParameters (..)
  , PoolRewardInfo (..)
  , Rewards
  , StakeDistribution
  , Delgations
  , Pools
  , BlocksMade
  ) where

import Data.Map (Map)
import Data.Monoid (Sum (..))
import Data.Ratio ((%))
import Data.Set (Set)

newtype Coin = Coin Integer
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid, Num) via Sum Integer

newtype BlockCount = BlockCount { unBlockCount :: Integer }
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid, Num) via Sum Integer

coinFloor :: Rational -> Coin
coinFloor = Coin . floor

coinToQ :: Coin -> Rational
coinToQ (Coin c) = fromIntegral c

coinRatio :: Coin -> Coin -> Rational
coinRatio (Coin x) (Coin y) = x % y

newtype StakeCredential c = StakeCredential c
  deriving (Show, Eq, Ord)

newtype PoolID p = PoolID p
  deriving (Show, Eq, Ord)

data PoolParameters c =
  PoolParameters
    { cost   :: Coin,
      margin :: Rational,
      pledge :: Coin,
      owners :: Set (StakeCredential c),
      poolStakeCred :: StakeCredential c
    } deriving (Show)

data ProtocolParameters =
  ProtocolParameters
    { asc  :: Rational
    , d    :: Rational
    , κ :: Integer
    , ρ    :: Rational
    , τ  :: Rational
    }

data PoolRewardInfo c =
  PoolRewardInfo
    { pooledStake :: StakeDistribution c
    , poolRelativeStake :: Rational
    , ownerStake :: Coin
    , poolPot :: Coin
    , poolPs :: PoolParameters c
    } deriving (Show)

type Rewards c           = Map (StakeCredential c) Coin
type StakeDistribution c = Map (StakeCredential c) Coin
type Delgations p c      = Map (StakeCredential c) (PoolID p)
type Pools p c           = Map (PoolID p) (PoolParameters c)
type BlocksMade p        = Map (PoolID p) BlockCount
