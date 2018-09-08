{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.LineString
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- Refer to the GeoJSON Spec <http://geojson.org/geojson-spec.html#linestring>
--
-- A LinearString is a List with at least 2 elements
--
-------------------------------------------------------------------
module Data.LineString (
    -- * Type
        LineString
    ,   ListToLineStringError(..)
    ,   VectorToLineStringError(..)
    -- * Functions
    ,   combineToVector
    ,   fromVector
    ,   fromLineString
    ,   fromList
    ,   makeLineString
    ,   lineStringHead
    ,   lineStringLast
    ,   lineStringLength
    ) where

import           Prelude             hiding (foldr)

import           Control.Applicative (Applicative (..))
import           Control.Lens        (( # ), (^?))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value)
import           Data.Aeson.Types    (Parser, typeMismatch)
import           Data.Foldable       (Foldable (..))
import           Data.Functor        ((<$>))
import           Data.Maybe          (fromMaybe)
import           Data.Traversable    (Traversable (..))
import           Data.Validation     (Validate (..), Validation, _Failure,
                                      _Success)
import qualified Data.Vector         as Vector

-- |
-- a LineString has at least 2 elements
--
data LineString a = LineString a a (Vector.Vector a)  deriving (Eq)

-- |
-- When converting a List to a LineString, here is a list of things that can go wrong:
--
--     * The list was empty
--     * The list only had one element
--
data ListToLineStringError =
        ListEmpty
    |   SingletonList
    deriving (Eq)

-- |
-- When converting a Vector to a LineString, here is a list of things that can go wrong:
--
--     * The vector was empty
--     * The vector only had one element
--
data VectorToLineStringError =
       VectorEmpty
   |   SingletonVector
   deriving (Eq)

-- functions

-- |
-- returns the element at the head of the string
--
lineStringHead :: LineString a -> a
lineStringHead (LineString x _ _) = x

-- |
-- returns the last element in the string
--
lineStringLast :: LineString a -> a
lineStringLast (LineString _ x xs) = fromMaybe x (safeLast xs)

-- |
-- returns the number of elements in the list, including the replicated element at the end of the list.
--
lineStringLength :: LineString a -> Int
lineStringLength (LineString _ _ xs) = 2 + length xs

-- |
-- This function converts it into a list and appends the given element to the end.
--
fromLineString :: LineString a -> [a]
fromLineString (LineString x y zs) = x : y : Vector.toList zs

-- |
-- creates a LineString out of a list of elements,
-- if there are enough elements (needs at least 2) elements
--
fromList :: (Validate v) => [a] -> v ListToLineStringError (LineString a)
fromList []       = _Failure # ListEmpty
fromList [_]      = _Failure # SingletonList
fromList (x:y:zs) = _Success # LineString x y (Vector.fromList zs)

-- |
-- create a vector from a LineString by combining values.
-- LineString 1 2 [3,4] (,) --> Vector [(1,2),(2,3),(3,4)]
--
combineToVector :: (a -> a -> b) -> LineString a -> Vector.Vector b
combineToVector combine (LineString a b rest) = Vector.cons (combine a b) combineRest
    where
        combineRest =
          if Vector.null rest
            then
              Vector.empty
            else
              (Vector.zipWith combine <*> Vector.tail) (Vector.cons b rest)

-- |
-- creates a LineString out of a vector of elements,
-- if there are enough elements (needs at least 2) elements
--
fromVector :: (Validate v) => Vector.Vector a -> v VectorToLineStringError (LineString a)
fromVector v =
  if Vector.null v then
    _Failure # VectorEmpty
  else
    fromVector' (Vector.head v) (Vector.take 1 v)

fromVector' :: (Validate v) => a -> Vector.Vector a -> v VectorToLineStringError (LineString a)
fromVector' first v =
  if Vector.null v then
    _Failure # SingletonVector
  else
    _Success # LineString first (Vector.head v) (Vector.take 1 v)

-- |
-- Creates a LineString
-- @makeLineString x y zs@ creates a `LineString` homomorphic to the list @[x, y] ++ zs@
--
makeLineString
    :: a            -- ^ The first element
    -> a            -- ^ The second element
    -> [a]          -- ^ The rest of the optional elements
    -> LineString a
makeLineString a b c = LineString a b (Vector.fromList c)

-- instances

instance Show ListToLineStringError where
    show ListEmpty     = "List Empty"
    show SingletonList = "Singleton List"

instance Show VectorToLineStringError where
  show VectorEmpty     = "Vector Empty"
  show SingletonVector = "Singleton Vector"

instance (Show a) => Show (LineString a) where
    show  = show . fromLineString

instance Functor LineString where
    fmap f (LineString x y zs) = LineString (f x) (f y) (fmap f zs)

-- | This instance of Foldable will run through the entire ring, closing the
-- loop by also passing the initial element in again at the end.
--
instance Foldable LineString where
--  foldr :: (a -> b -> b) -> b -> LineString a -> b
    foldr f u (LineString x y zs) = f x (f y (foldr f u zs))

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
        maybe (parseError v (vxs ^? _Failure)) return (vxs ^? _Success)

-- helpers

fromListValidated :: [a] -> Validation ListToLineStringError (LineString a)
fromListValidated = fromList

parseError :: Value -> Maybe ListToLineStringError -> Parser b
parseError v = maybe mzero (\e -> typeMismatch (show e) v)

safeLast :: Vector.Vector a -> Maybe a
safeLast x = if Vector.null x then Nothing else Just $ Vector.last x
