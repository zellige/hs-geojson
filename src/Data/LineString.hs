{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------

-- |
-- Module       : Data.LineString
-- Copyright    : (C) 2014-2021 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- Refer to the GeoJSON Spec <http://geojson.org/geojson-spec.html#linestring>
--
-- A LinearString is a List with at least 2 elements
module Data.LineString
  ( -- * Type
    LineString,
    ListToLineStringError (..),
    SequenceToLineStringError (..),

    -- * Functions
    toSeq,
    combineToSeq,
    fromSeq,
    fromLineString,
    fromList,
    makeLineString,
    lineStringHead,
    lineStringLast,
    lineStringLength,
  )
where

import Control.DeepSeq
import Control.Lens ((#), (^?))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Foldable as Foldable
import Data.Maybe (fromMaybe)
import qualified Data.SeqHelper as SeqHelper
import qualified Data.Sequence as Sequence
import qualified Data.Validation as Validation
import GHC.Generics (Generic)
import Prelude hiding (foldr)

-- |
-- a LineString has at least 2 elements
data LineString a = LineString a a (Sequence.Seq a) deriving (Eq, Generic, NFData)

-- |
-- When converting a List to a LineString, here is a list of things that can go wrong:
--
--     * The list was empty
--     * The list only had one element
data ListToLineStringError
  = ListEmpty
  | SingletonList
  deriving (Eq)

-- |
-- When converting a Sequence to a LineString, here is a list of things that can go wrong:
--
--     * The sequence was empty
--     * The sequence only had one element
data SequenceToLineStringError
  = SequenceEmpty
  | SingletonSequence
  deriving (Eq)

-- functions

-- |
-- returns the element at the head of the string
lineStringHead :: LineString a -> a
lineStringHead (LineString x _ _) = x

-- |
-- returns the last element in the string
lineStringLast :: LineString a -> a
lineStringLast (LineString _ x xs) = fromMaybe x (safeLast xs)

-- |
-- returns the number of elements in the list, including the replicated element at the end of the list.
lineStringLength :: LineString a -> Int
lineStringLength (LineString _ _ xs) = 2 + Sequence.length xs

-- |
-- This function converts it into a list and appends the given element to the end.
fromLineString :: LineString a -> [a]
fromLineString (LineString x y zs) = x : y : Foldable.toList zs

-- |
-- creates a LineString out of a list of elements,
-- if there are enough elements (needs at least 2) elements
fromList :: (Validation.Validate v) => [a] -> v ListToLineStringError (LineString a)
fromList [] = Validation._Failure # ListEmpty
fromList [_] = Validation._Failure # SingletonList
fromList (x : y : zs) = Validation._Success # LineString x y (Sequence.fromList zs)
{-# INLINE fromList #-}

-- |
-- create a sequence from a LineString by combining values.
-- LineString 1 2 [3,4] (,) --> Sequence [(1,2),(2,3),(3,4)]
combineToSeq :: (a -> a -> b) -> LineString a -> Sequence.Seq b
combineToSeq combine (LineString a b rest) = combine a b Sequence.<| combineRest
  where
    combineRest =
      if Sequence.null rest
        then Sequence.empty
        else (Sequence.zipWith combine <*> SeqHelper.sequenceTail) (b Sequence.<| rest)
{-# INLINE combineToSeq #-}

-- |
-- create a sequence from a LineString.
-- LineString 1 2 [3,4] --> Sequence [1,2,3,4]
toSeq :: LineString a -> Sequence.Seq a
toSeq (LineString a b rest) = a Sequence.<| (b Sequence.<| rest)
{-# INLINE toSeq #-}

-- |
-- creates a LineString out of a sequence of elements,
-- if there are enough elements (needs at least 2) elements
fromSeq :: (Validation.Validate v) => Sequence.Seq a -> v SequenceToLineStringError (LineString a)
fromSeq v@(headS Sequence.:<| tailS) =
  if Sequence.null v
    then Validation._Failure # SingletonSequence
    else fromSeq' headS tailS
fromSeq _ = Validation._Failure # SequenceEmpty
{-# INLINE fromSeq #-}

fromSeq' :: (Validation.Validate v) => a -> Sequence.Seq a -> v SequenceToLineStringError (LineString a)
fromSeq' first v@(headS Sequence.:<| tailS) =
  if Sequence.null v
    then Validation._Failure # SingletonSequence
    else Validation._Success # LineString first headS tailS
fromSeq' _ _ = Validation._Failure # SingletonSequence
{-# INLINE fromSeq' #-}

-- |
-- Creates a LineString
-- @makeLineString x y zs@ creates a `LineString` homomorphic to the list @[x, y] ++ zs@
makeLineString ::
  -- | The first element
  a ->
  -- | The second element
  a ->
  -- | The rest of the optional elements
  Sequence.Seq a ->
  LineString a
makeLineString = LineString

-- instances

instance Show ListToLineStringError where
  show ListEmpty = "List Empty"
  show SingletonList = "Singleton List"

instance Show SequenceToLineStringError where
  show SequenceEmpty = "Sequence Empty"
  show SingletonSequence = "Singleton Sequence"

instance (Show a) => Show (LineString a) where
  show = show . fromLineString

instance Functor LineString where
  fmap f (LineString x y zs) = LineString (f x) (f y) (fmap f zs)

-- | This will run through the line string.
instance Foldable LineString where
  --  foldr :: (a -> b -> b) -> b -> LineString a -> b
  foldr f u (LineString x y zs) = f x (f y (Foldable.foldr f u zs))

  -- we implement these methods for improved performance
  null _ = False
  length (LineString _ _ zs) = 2 + Sequence.length zs


instance Traversable LineString where
  --  sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
  sequenceA (LineString fx fy fzs) = LineString <$> fx <*> fy <*> sequenceA fzs

instance (ToJSON a) => ToJSON (LineString a) where
  --  toJSON :: a -> Value
  toJSON = toJSON . fromLineString

instance (FromJSON a, Show a) => FromJSON (LineString a) where
  --  parseJSON :: Value -> Parser a
  parseJSON v = do
    xs <- parseJSON v
    let vxs = fromListValidated xs
    maybe (parseError v (vxs ^? Validation._Failure)) return (vxs ^? Validation._Success)

-- helpers

fromListValidated :: [a] -> Validation.Validation ListToLineStringError (LineString a)
fromListValidated = fromList

parseError :: Value -> Maybe ListToLineStringError -> Parser b
parseError v = maybe mzero (\e -> typeMismatch (show e) v)

-- safeLast :: (Sequence.Seq a) => Sequence.Seq a -> Maybe a
-- safeLast x = if Sequence.null x then Nothing else Just $ Sequence.last x
safeLast :: Sequence.Seq a -> Maybe a
safeLast x = case Sequence.viewr x of
  Sequence.EmptyR -> Nothing
  _ Sequence.:> b -> Just b
{-# INLINE safeLast #-}
