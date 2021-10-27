module Main where

import Cardano.Rewards.Original as Original (rewards)
import Cardano.Rewards.Refined as Refined (rewards)
import Cardano.Rewards.Types
import Test.Cardano.Rewards.Arbitrary (Example (..))

import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.QuickCheck

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

maxSupplyEx :: Coin
maxSupplyEx = 1000000

slotsPerEpochEx :: Integer
slotsPerEpochEx = 100

testEquality :: (Show c, Ord p, Ord c) => Example p c -> Property
testEquality (Example pp pools blocks delegations stake fees reserves maxSupply slotsPerEpoch) =
  removeZeros results1 === removeZeros results2
  where
    results1 = Original.rewards pp pools blocks delegations stake fees reserves maxSupply slotsPerEpoch
    results2 = Refined.rewards pp pools blocks delegations stake fees reserves maxSupply slotsPerEpoch
    removeZeros = Map.filter (/= 0)

ex1 :: Property
ex1 = testEquality (Example ppEx poolsEx blocksEx delegationsEx stakeEx feesEx reservesEx maxSupplyEx slotsPerEpochEx)

tests :: TestTree
tests = testGroup "Cardano Rewards"
  [ testProperty "equality example" ex1
  , testProperty "equality prop" (testEquality :: Example Int Int -> Property)
  ]

main :: IO ()
main = defaultMain tests
