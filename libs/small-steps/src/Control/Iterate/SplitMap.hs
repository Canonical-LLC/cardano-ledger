{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Iterate.SplitMap where

import Control.Iterate.Collect
import Control.Iterate.SetAlgebra
import qualified Data.Compact.KeyMap as KeyMap
import Data.Compact.SplitMap (Split (..), SplitMap (..))
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

-- ======================================

-- | Insert a KeyMap into an IntMap, unless it is empty, if so, return the IntMap unchanged
--   we assume the Int 'n' is not already in the IntMap 'imap', and each call site should have this invariant.
insertNormForm :: Split k => IntMap.Key -> KeyMap.KeyMap v -> IntMap.IntMap (KeyMap.KeyMap v) -> SplitMap k v
insertNormForm _ (KeyMap.Empty) imap = SplitMap imap
insertNormForm n kmap imap = SplitMap (IntMap.insert n kmap imap)

instance Iter SplitMap where
  nxt (SplitMap imap) =
    case IntMap.minViewWithKey imap of
      Nothing -> none
      Just ((n, kmap), imap2) ->
        case KeyMap.minViewWithKey kmap of
          Nothing -> none -- This should never happen, every 'n' should have at least one 'key'
          Just ((key, v), kmap2) -> one (join n key, v, insertNormForm n kmap2 imap2)

  lub k (SplitMap imap) =
    let (n, key) = split k
     in case IntMap.splitLookup n imap of
          (_, Just kmap, imap2) ->
            case KeyMap.lub key kmap of
              Nothing -> none -- This should never happen, every 'n' should have at least one 'key'
              Just (key2, v, kmap2) -> one (join n key2, v, insertNormForm n kmap2 imap2)
          (_, Nothing, imap3) -> nxt (SplitMap imap3)

  isnull (SplitMap x) = IntMap.null x

  haskey k x =
    case SplitMap.lookup k x of
      Nothing -> False
      Just _ -> True

  lookup k x = SplitMap.lookup k x

  element k x =
    case SplitMap.lookup k x of
      Nothing -> none
      Just _ -> one ()

instance Basic SplitMap where
  addpair k v x = SplitMap.insert k v x
  addkv (k, v) x comb = SplitMap.insertWith comb k v x
  removekey = SplitMap.delete
  domain smap = SplitMap.foldlWithKey' accum Set.empty smap
    where
      accum ans k _ = Set.insert k ans
  range smap = SplitMap.foldlWithKey' accum Set.empty smap
    where
      accum ans _ v = Set.insert v ans
