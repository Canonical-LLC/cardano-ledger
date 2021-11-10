{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Sharing
  ( FromSharedCBOR (..),
    Interns (..),
    Intern (..),
    fromSharedLensCBOR,
    fromSharedPlusCBOR,
    fromSharedPlusLensCBOR,
    fromNotSharedCBOR,
    interns,
    internsFromMap,
    internsFromVMap,
    toMemptyLens,
  )
where

import Cardano.Binary (Decoder, FromCBOR (..), decodeListLen, dropMap)
import Control.Iterate.SetAlgebra (BiMap (..), biMapFromMap)
import Control.Monad (void)
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Coders (decodeMap, decodeVMap, invalidKey)
import Data.Compact.VMap (VB, VMap, VP)
import qualified Data.Compact.VMap as VMap
import qualified Data.Foldable as F
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Internal
import Data.Primitive.Types (Prim)
import Lens.Micro

-- =======================================

data Intern a = Intern
  { internMaybe :: a -> Maybe a,
    -- | Used for sorting
    internWeight :: Int
  }

newtype Interns a = Interns [Intern a]
  deriving (Monoid)

interns :: Interns k -> k -> k
interns (Interns []) !k = k -- opimize for common case when there are no interns
interns (Interns is) !k = go is
  where
    go [] = k
    go (x : xs) =
      case internMaybe x k of
        Just kx -> kx
        Nothing -> go xs
{-# INLINE interns #-}

internsFromMap :: Ord k => Map k a -> Interns k
internsFromMap m =
  Interns
    [ Intern
        { internMaybe = \k ->
            let go Tip = Nothing
                go (Bin _ kx _ l r) =
                  case compare k kx of
                    LT -> go l
                    GT -> go r
                    EQ -> Just kx
             in go m,
          internWeight = Map.size m
        }
    ]

internsFromVMap :: Ord k => VMap VB kv k a -> Interns k
internsFromVMap m =
  Interns
    [ Intern
        { internMaybe = \k -> VMap.internMaybe k m,
          internWeight = VMap.size m
        }
    ]

instance Semigroup (Interns a) where
  (<>) is1 (Interns []) = is1
  (<>) (Interns []) is2 = is2
  (<>) (Interns is1) (Interns is2) =
    Interns (F.foldl' insertIntoSortedInterns is2 is1)
    where
      insertIntoSortedInterns [] i = [i]
      insertIntoSortedInterns (a : as) i
        | internWeight a > internWeight i = a : insertIntoSortedInterns as i
        | otherwise = i : a : as

class Monoid (Share a) => FromSharedCBOR a where
  type Share a :: Type
  type Share a = ()

  getShare :: a -> Maybe (Share a)
  getShare _ = Nothing

  fromSharedCBOR :: StateT (Share a) (Decoder s) a

-- | Deserialize with sharing and add the state that used for sharing
fromSharedPlusCBOR :: FromSharedCBOR a => StateT (Share a) (Decoder s) a
fromSharedPlusCBOR = do
  x <- fromSharedCBOR
  x <$ mapM_ (\s -> modify' (s <>)) (getShare x)

fromSharedLensCBOR ::
  FromSharedCBOR b =>
  SimpleGetter bs (Share b) ->
  StateT bs (Decoder s) b
fromSharedLensCBOR l = do
  s <- get
  lift $ evalStateT fromSharedCBOR (s ^. l)

-- | Using this function it is possible to compose two lenses. One will
-- extract a value and another will used it for placing it into a empty monoid.
toMemptyLens :: Monoid a => Lens' a b -> Lens' c b -> Lens' c a
toMemptyLens lto lfrom =
  lens (\s -> mempty & lto .~ (s ^. lfrom)) (\s a -> s & lfrom .~ (a ^. lto))

-- | Just like `fromSharedPlusCBOR`, except allows to transform the shared state
-- with a lens.
fromSharedPlusLensCBOR ::
  FromSharedCBOR b =>
  Lens' bs (Share b) ->
  StateT bs (Decoder s) b
fromSharedPlusLensCBOR l = do
  s <- get
  (x, k) <- lift $ runStateT fromSharedPlusCBOR (s ^. l)
  x <$ put (s & l .~ k)

-- | Use `FromSharedCBOR` class while ingoring sharing
fromNotSharedCBOR :: FromSharedCBOR a => Decoder s a
fromNotSharedCBOR = evalStateT fromSharedCBOR mempty

instance (Ord k, FromCBOR k, FromCBOR v) => FromSharedCBOR (Map k v) where
  type Share (Map k v) = (Interns k, Interns v)
  fromSharedCBOR = do
    (kis, vis) <- get
    lift $ decodeMap (interns kis <$> fromCBOR) (interns vis <$> fromCBOR)
  getShare m = Just (internsFromMap m, mempty)

instance (Ord k, FromCBOR k, FromCBOR v) => FromSharedCBOR (VMap VB VB k v) where
  type Share (VMap VB VB k v) = (Interns k, Interns v)
  fromSharedCBOR = do
    (kis, vis) <- get
    lift $ decodeVMap (interns kis <$> fromCBOR) (interns vis <$> fromCBOR)
  getShare m = Just (internsFromVMap m, mempty)

instance (Ord k, FromCBOR k, FromCBOR v, Prim v) => FromSharedCBOR (VMap VB VP k v) where
  type Share (VMap VB VP k v) = Interns k
  fromSharedCBOR = do
    kis <- get
    lift $ decodeVMap (interns kis <$> fromCBOR) fromCBOR
  getShare m = Just (internsFromVMap m)

-- ==============================================================================
-- These BiMap instances are adapted from the FromCBOR instances in Data.Coders

instance (Ord a, Ord b, FromCBOR a, FromCBOR b) => FromSharedCBOR (BiMap b a b) where
  type Share (BiMap b a b) = (Interns a, Interns b)
  fromSharedCBOR =
    lift decodeListLen >>= \case
      1 -> biMapFromMap <$> fromSharedCBOR
      -- Previous encoding of 'BiMap' encoded both the forward and reverse
      -- directions. In this case we skip the reverse encoding. Note that,
      -- further, the reverse encoding was from 'b' to 'a', not the current 'b'
      -- to 'Set a', and hence the dropper reflects that.
      2 -> do
        !x <- biMapFromMap <$> fromSharedCBOR
        lift $ dropMap (void $ fromCBOR @b) (void $ fromCBOR @a)
        return x
      k -> lift $ invalidKey (fromIntegral k)
  getShare (MkBiMap m1 m2) = Just (internsFromMap m1, internsFromMap m2)
