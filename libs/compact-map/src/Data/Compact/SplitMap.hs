{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Compact.SplitMap where

import Data.Compact.KeyMap (Key, KeyMap)
import qualified Data.Compact.KeyMap as KeyMap
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap)

class Split k where
  split :: k -> (Int, Key)
  join :: Int -> Key -> k

data SplitMap k v where
  SplitMap :: Split k => IntMap (KeyMap v) -> SplitMap k v

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
