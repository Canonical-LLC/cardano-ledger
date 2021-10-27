{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Rewards.Arbitrary where

import Data.Ratio ((%))
import Data.Word (Word64)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty.QuickCheck
import Cardano.Rewards.Types

instance Arbitrary a => Arbitrary (StakeCredential a) where
  arbitrary = StakeCredential <$> arbitrary

instance Arbitrary a => Arbitrary (PoolID a) where
  arbitrary = PoolID <$> arbitrary

instance Arbitrary Coin where
  arbitrary = (Coin . fromIntegral) <$> (arbitrary :: Gen Word64)

pickOwners :: Ord a => [StakeCredential a] -> Gen (Set (StakeCredential a))
pickOwners creds = Set.fromList <$> resize 3 (sublistOf creds)

genPoolParams :: Ord a => [StakeCredential a] -> Gen (PoolParameters a)
genPoolParams creds = do
  c <- arbitrary
  m <- choosePct
  p <- arbitrary
  os <- pickOwners creds
  psc <- elements creds
  pure $
    PoolParameters
      { cost   = c
      , margin = m
      , pledge = p
      , owners = os
      , poolStakeCred = psc
      }

instance Arbitrary BlockCount where
  arbitrary = (BlockCount . fromIntegral) <$> (arbitrary :: Gen Word64)

genMapFromKeyList :: Ord a => Gen b -> [a] -> Gen (Map a b)
genMapFromKeyList g as = Map.fromList <$> (mapM (genAttach g) as)
  where genAttach h a = (a,) <$> h

genPools :: (Ord p, Ord a) => [PoolID p] -> [StakeCredential a] -> Gen (Pools p a)
genPools pids creds = genMapFromKeyList (genPoolParams creds) pids

genBlocks :: Ord p => [PoolID p] -> Gen (BlocksMade p)
genBlocks pids = sublistOf pids >>= genMapFromKeyList arbitrary

genDelegs :: Ord a => [PoolID p] -> [StakeCredential a] -> Gen (Delgations p a)
genDelegs pids creds = genMapFromKeyList (elements pids) creds

genStakeDstr :: Ord a => [StakeCredential a] -> Gen (StakeDistribution a)
genStakeDstr creds = genMapFromKeyList arbitrary creds

choosePct :: Gen Rational
choosePct = (% 100) <$> choose (0, 100)

instance Arbitrary ProtocolParameters where
  arbitrary =
    ProtocolParameters
      <$> choosePct -- asc
      <*> choosePct -- d
      <*> (choose (1, 1000000)) -- κ
      <*> choosePct -- ρ
      <*> choosePct -- τ

data Example p c =
  Example
    ProtocolParameters
    (Pools p c)
    (BlocksMade p)
    (Delgations p c)
    (StakeDistribution c)
    Coin
    Coin
    Coin
    Integer
  deriving (Show)

instance (Ord p, Arbitrary p, Ord c, Arbitrary c) =>
  Arbitrary (Example p c) where
  arbitrary = do
    pp <- arbitrary
    pids <- (Set.toList . Set.fromList) <$> (listOf1 arbitrary)
    creds <- (Set.toList . Set.fromList) <$> (listOf1 arbitrary)
    pools <- genPools pids creds
    blocks <- genBlocks pids
    delegs <- genDelegs pids creds
    stakeDistr <- genStakeDstr creds
    stakeCred <- arbitrary
    reserves <- arbitrary
    maxSupply <- arbitrary
    let stakeDistr' = Map.insertWith (<>) stakeCred 1 stakeDistr -- keep the stake non-zero
        totalActive = sum stakeDistr'
        reserves' = reserves <> 100
        maxSupply' = totalActive <> reserves' <> maxSupply
    fee <- arbitrary
    slotsPerEpoch <- choose (100, 1000)
    pure $ Example
             pp
             pools
             blocks
             delegs
             stakeDistr'
             fee
             reserves'
             maxSupply'
             slotsPerEpoch
