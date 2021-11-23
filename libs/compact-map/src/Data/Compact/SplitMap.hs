{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Compact.SplitMap where

import Data.Compact.KeyMap (Key, KeyMap)
import qualified Data.Compact.KeyMap as KeyMap
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (lookup)

class Split k where
  split :: k -> (Int, Key)
  join :: Int -> Key -> k

data SplitMap k v where
  SplitMap :: Split k => IntMap (KeyMap v) -> SplitMap k v

empty :: forall k v. Split k => SplitMap k v
empty = SplitMap IntMap.empty

insertWithKey :: forall k v. (k -> v -> v -> v) -> k -> v -> SplitMap k v -> SplitMap k v
insertWithKey combine k v (SplitMap imap) = SplitMap (IntMap.insertWith combine2 n (KeyMap.insert key v KeyMap.Empty) imap)
  where
    (n, key) = split k
    combine2 :: KeyMap v -> KeyMap v -> KeyMap v
    combine2 km1 km2 = KeyMap.unionWith (combine k) km1 km2

insertWith :: forall k v. (v -> v -> v) -> k -> v -> SplitMap k v -> SplitMap k v
insertWith comb k v mp = insertWithKey (\_ x y -> comb x y) k v mp

insert :: forall k v. k -> v -> SplitMap k v -> SplitMap k v
insert k v mp = insertWithKey (\_k v1 _v2 -> v1) k v mp

delete :: k -> SplitMap k v -> SplitMap k v
delete k (SplitMap imap) = SplitMap (IntMap.update fix n imap)
  where
    (n, key) = split k
    fix keymap = case KeyMap.delete key keymap of
      KeyMap.Empty -> Nothing
      !other -> Just other

lookup :: k -> SplitMap k v -> Maybe v
lookup k (SplitMap imap) =
  case IntMap.lookup n imap of
    Nothing -> Nothing
    Just keymap -> KeyMap.lookupHM key keymap
  where
    (n, key) = split k

mapWithKey :: forall k v u. (k -> v -> u) -> SplitMap k v -> SplitMap k u
mapWithKey f (SplitMap imap) = SplitMap (IntMap.mapWithKey g imap)
  where
    g :: Int -> KeyMap v -> KeyMap u
    g n kmap = KeyMap.mapWithKey (\key v -> f (join n key) v) kmap

unionWithKey :: forall k v. (k -> v -> v -> v) -> SplitMap k v -> SplitMap k v -> SplitMap k v
unionWithKey combine (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.unionWithKey comb imap1 imap2)
  where
    comb :: Int -> KeyMap v -> KeyMap v -> KeyMap v
    comb n x y = KeyMap.unionWithKey (\key v1 v2 -> combine (join n key) v1 v2) x y

unionWith :: forall k v. (v -> v -> v) -> SplitMap k v -> SplitMap k v -> SplitMap k v
unionWith combine (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.unionWith comb imap1 imap2)
  where
    comb :: KeyMap v -> KeyMap v -> KeyMap v
    comb x y = KeyMap.unionWith combine x y

union :: forall k v. SplitMap k v -> SplitMap k v -> SplitMap k v
union (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.unionWith comb imap1 imap2)
  where
    comb :: KeyMap v -> KeyMap v -> KeyMap v
    comb x y = KeyMap.unionWith (\v _ -> v) x y

-- ============================================================================

foldlWithKey' :: forall k v ans. (ans -> k -> v -> ans) -> ans -> SplitMap k v -> ans
foldlWithKey' comb ans0 (SplitMap imap) = IntMap.foldlWithKey' comb2 ans0 imap
  where
    comb2 :: ans -> Int -> KeyMap v -> ans
    comb2 ans1 n kmap = KeyMap.foldWithDescKey comb3 ans1 kmap
      where
        comb3 :: Key -> v -> ans -> ans
        comb3 key v ans2 = comb ans2 (join n key) v

foldrWithKey' :: forall k v ans. (k -> ans -> v -> ans) -> ans -> SplitMap k v -> ans
foldrWithKey' comb ans0 (SplitMap imap) = IntMap.foldrWithKey' comb2 ans0 imap
  where
    comb2 :: Int -> KeyMap v -> ans -> ans
    comb2 n kmap ans1 = KeyMap.foldWithAscKey comb3 ans1 kmap
      where
        comb3 :: ans -> Key -> v -> ans
        comb3 ans2 key v = comb (join n key) ans2 v

-- =================================================================================
-- These 'restrictKeys' functions assume the structure holding the 'good' keys is small
-- An alternate approach is to use cross-type 'intersection' operations

restrictKeysSet :: forall k a. SplitMap k a -> Set k -> SplitMap k a
restrictKeysSet splitmap@(SplitMap _) kset = Set.foldl' comb (SplitMap IntMap.empty) kset
  where
    comb :: SplitMap k a -> k -> SplitMap k a
    comb smap k = case lookup k splitmap of
      Nothing -> smap
      Just a -> insert k a smap

restrictKeysMap :: forall k a b. SplitMap k a -> Map k b -> SplitMap k a
restrictKeysMap splitmap@(SplitMap _) kmap = Map.foldlWithKey' comb (SplitMap IntMap.empty) kmap
  where
    comb :: SplitMap k a -> k -> b -> SplitMap k a
    comb smap k _ = case lookup k splitmap of
      Nothing -> smap
      Just a -> insert k a smap

restrictKeysSplit :: forall k a b. SplitMap k a -> SplitMap k b -> SplitMap k a
restrictKeysSplit splitmap@(SplitMap _) ksplit = foldlWithKey' comb (SplitMap IntMap.empty) ksplit
  where
    comb :: SplitMap k a -> k -> b -> SplitMap k a
    comb smap k _ = case lookup k splitmap of
      Nothing -> smap
      Just a -> insert k a smap

-- =================================================================================
-- These 'withoutKeys' functions assume the structure holding the 'bad' keys is small
-- An alternate approach is to use cross-type 'intersection' operations

withoutKeysSet :: forall k a. SplitMap k a -> Set k -> SplitMap k a
withoutKeysSet splitmap@(SplitMap _) kset = Set.foldl' comb splitmap kset
  where
    comb :: SplitMap k a -> k -> SplitMap k a
    comb smap k = delete k smap

withoutKeysMap :: forall k a b. SplitMap k a -> Map k b -> SplitMap k a
withoutKeysMap splitmap@(SplitMap _) kset = Map.foldlWithKey' comb splitmap kset
  where
    comb :: SplitMap k a -> k -> b -> SplitMap k a
    comb smap k _ = delete k smap

withoutKeysSplit :: forall k a b. SplitMap k a -> SplitMap k b -> SplitMap k a
withoutKeysSplit splitmap@(SplitMap _) kset = foldlWithKey' comb splitmap kset
  where
    comb :: SplitMap k a -> k -> b -> SplitMap k a
    comb smap k _ = delete k smap
