{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.Map as Map
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.QuickCheck

import Cardano.Rewards
import Test.Cardano.Rewards.Arbitrary (RewardParameters (..))

testEquality :: (Show c, Ord p, Ord c) => RewardParameters p c -> Property
testEquality (RewardParameters pp pools blocks delegations stake fees reserves maxSupply slotsPerEpoch) =
  withMaxSuccess 10000 $ removeZeros results1 === removeZeros results2
  where
    results1 = rewardsA pp pools blocks delegations stake fees reserves maxSupply slotsPerEpoch
    results2 = rewardsB pp pools blocks delegations stake fees reserves maxSupply slotsPerEpoch
    removeZeros = Map.filter (/= 0)

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
       , k = 2
       , rho = 1 % 100
       , tau = 20 % 100
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

maxSupplyEx :: Coin
maxSupplyEx = 45000000

slotsPerEpochEx :: Integer
slotsPerEpochEx = 100

ex1 :: Property
ex1 = testEquality $
        RewardParameters
          ppEx
          poolsEx
          blocksEx
          delegationsEx
          stakeEx
          feesEx
          reservesEx
          maxSupplyEx
          slotsPerEpochEx

instance Arbitrary Natural where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word64)

tests :: TestTree
tests = testGroup "Cardano Rewards"
  [ testProperty "equality example" ex1
  , testProperty "equality prop" (testEquality :: RewardParameters Natural Natural -> Property)
  ]

main :: IO ()
main = defaultMain tests
