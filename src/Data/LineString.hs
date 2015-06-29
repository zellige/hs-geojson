{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.LineString
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
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
    -- * Functions
    ,   fromLineString
    ,   fromList
    ,   makeLineString
    ,   lineStringHead
    ,   lineStringLast
    ,   lineStringLength
    ) where

import Prelude hiding ( foldr )

import Control.Applicative ( Applicative(..) )
import Control.Lens ( ( # ), (^?) )
import Control.Monad ( mzero )
import Data.Aeson ( ToJSON(..), FromJSON(..), Value )
import Data.Aeson.Types ( Parser, typeMismatch )
import Data.Foldable ( Foldable(..) )
import Data.Functor ( (<$>) )
import Data.Maybe ( fromMaybe )
import Data.Traversable ( Traversable(..) )
import Data.Validation ( Validate(..), Validation, _Failure, _Success )

-- $setup
--
-- >>> import Control.Applicative ( (<*>) )
-- >>> import Data.Functor ( (<$>) )
-- >>> import Data.Maybe ( Maybe(..) )
-- >>> import Data.Monoid ( Monoid(..) )
-- >>> import Test.QuickCheck
--
-- >>> instance (Arbitrary a) => Arbitrary (LineString a) where arbitrary = makeLineString <$> arbitrary <*> arbitrary <*> arbitrary
--
--

-- |
-- a LineString has at least 2 elements
--
data LineString a = LineString a a [a] deriving (Eq)

-- |
-- When converting a List to a LineString, here is a list of things that can go wrong:
--
--     * The list was empty
--     * The list only had one element
--
data ListToLineStringError =
        ListEmpty
    |   SingletonList

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

-- NOTE (Dom De Re): Props have been commented out until <https://github.com/sol/doctest-haskell/issues/83>
-- has been resolved.

-- |
-- returns the number of elements in the list, including the replicated element at the end of the list.
--
-- (\xs -> lineStringLength xs == (length (fromLineString xs))) (xs :: LineString Int)
--
lineStringLength :: LineString a -> Int
lineStringLength (LineString _ _ xs) = 2 + length xs

-- |
-- This function converts it into a list and appends the given element to the end.
--
-- (\xs -> safeLast (fromLineString xs) == Just (lineStringHead xs)) (xs :: LineString Int)
--
-- (\xs -> length (fromLineString xs) >= 4) (xs :: LineString Int)
--

fromLineString :: LineString a -> [a]
fromLineString (LineString x y zs) = x : y : zs

-- |
-- creates a LineString out of a list of elements,
-- if there are enough elements (needs at least 2) elements
--
-- >>> fromList [] :: Validation ListToLineStringError (LineString Int)
-- Failure List Empty
--
-- >>> fromList [0] :: Validation ListToLineStringError (LineString Int)
-- Failure Singleton List
--
-- >>> fromList [0, 1] :: Validation ListToLineStringError (LineString Int)
-- Success [0,1]
--
-- >>> fromList [0, 1, 2] :: Validation ListToLineStringError (LineString Int)
-- Success [0,1,2]
--
-- >>> fromList [0, 1, 2, 4, 5, 0] :: Validation ListToLineStringError (LineString Int)
-- Success [0,1,2,4,5,0]
--
fromList
    :: (Validate v)
    => [a]
    -> v ListToLineStringError (LineString a)
fromList []       = _Failure # ListEmpty
fromList [_]      = _Failure # SingletonList
fromList (x:y:zs) = _Success # LineString x y zs

-- |
-- Creates a LineString
-- @makeLineString x y zs@ creates a `LineString` homomorphic to the list @[x, y] ++ zs@
--
makeLineString
    :: a            -- ^ The first element
    -> a            -- ^ The second element
    -> [a]          -- ^ The rest of the optional elements
    -> LineString a
makeLineString = LineString


-- instances

instance Show ListToLineStringError where
    show ListEmpty = "List Empty"
    show SingletonList = "Singleton List"

instance (Show a) => Show (LineString a) where
    show  = show . fromLineString

instance Functor LineString where
    fmap f (LineString x y zs) = LineString (f x) (f y) (fmap f zs)

-- | This instance of Foldable will run through the entire ring, closing the
-- loop by also passing the initial element in again at the end.
--
-- > (\xs -> (foldr (:) [] xs) == (fromLineString xs)) (xs :: LineString Int)
--
-- > (\xs -> (lineStringHead xs) == (foldr'' (\a -> const a) 0 xs)) (xs :: LineString Int)
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

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs
