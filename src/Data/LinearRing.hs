{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.LinearRing
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Refer to the GeoJSON Spec <http://geojson.org/geojson-spec.html#polygon>
--
-- A LinearRing is a List with at least 4 elements, where the
-- first element is expected to be the same as the last.
--
-------------------------------------------------------------------
module Data.LinearRing (
    -- * Type
        LinearRing
    -- * Functions
    ,   fromLinearRing
    ,   fromList
    ,   fromListWithEqCheck
    ,   makeLinearRing
    ,   ringHead
    ) where

import Prelude hiding ( foldr )

import Control.Applicative ( Applicative(..) )
import Control.Lens ( ( # ), (^?) )
import Control.Monad ( Monad(..), join, mzero, sequence )
import Data.Aeson ( ToJSON(..), FromJSON(..), Value )
import Data.Aeson.Types ( Parser, typeMismatch )
import Data.Foldable ( Foldable(..) )
import Data.Function ( on )
import Data.Functor ( (<$>) )
import Data.List.NonEmpty ( NonEmpty, intersperse )
import Data.Maybe ( maybe )
import Data.Text ( Text, pack )
import Data.Traversable ( Traversable(..) )
import Data.Validation ( Validate(..), AccValidation )
import qualified Data.Vector as V

-- $setup
--
-- >>> import Control.Applicative ( (<*>) )
-- >>> import Data.Functor ( (<$>) )
-- >>> import Data.Maybe ( Maybe(..) )
-- >>> import Data.Monoid ( Monoid(..) )
-- >>> import Test.QuickCheck
--
-- >>> instance (Arbitrary a) => Arbitrary (LinearRing a) where arbitrary = makeLinearRing <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--
--

-- |
-- a LinearRing has at least 3 (distinct) elements
--
data LinearRing a =
        ThreePoints a a a
    |   a :| (LinearRing a)

infixr 5 :|

-- | when converting a List to a LinearRing there are some things that can go wrong
--
data ListToLinearRingError a =
        ListTooShort Int
    |   HeadNotEqualToLast a a

-- functions

-- |
-- returns the element at the head of the ring
--
ringHead :: LinearRing a -> a
ringHead (ThreePoints x _ _)    = x
ringHead (x :| _)               = x

-- NOTE (Dom De Re): Props have been commented out until <https://github.com/sol/doctest-haskell/issues/83>
-- has been resolved.

-- |
-- returns the number of elements in the list, including the replicated element at the end of the list.
--
-- (\xs -> ringLength xs == (length (fromLinearRing xs))) (xs :: LinearRing Int)
--
ringLength :: LinearRing a -> Int
ringLength (ThreePoints {}) = 4
ringLength (_ :| xs) = 1 + ringLength xs

-- |
-- This function converts it into a list and appends the given element to the end.
--
-- (\xs -> safeLast (fromLinearRing xs) == Just (ringHead xs)) (xs :: LinearRing Int)
--
-- (\xs -> length (fromLinearRing xs) >= 4) (xs :: LinearRing Int)
--

fromLinearRing :: LinearRing a -> [a]
fromLinearRing xs = foldr'' (:) [ringHead xs] xs

-- |
-- creates a LinearRing out of a list of elements,
-- if there arent enough elements (needs at least 4) elements
--
-- This version doesnt check equality of the head and tail in case
-- you wish to use it for elements with no Eq instance defined.
--
-- Also its a list, finding the last element could be expensive with large
-- lists.  So just follow the spec and make sure the ring is closed.
--
-- Ideally the Spec would be modified to remove the redundant last element from the Polygons/LineRings.
-- Its just going to waste bandwidth...
--
-- And be aware that the last element of the list will be dropped.
--
fromList
    :: (Validate v, Functor (v (NonEmpty (ListToLinearRingError a))))
    => [a]
    -> v (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromList []             = _Failure # return (ListTooShort 0)
fromList (_:[])         = _Failure # return (ListTooShort 1)
fromList (_:_:[])       = _Failure # return (ListTooShort 2)
fromList (_:_:_:[])     = _Failure # return (ListTooShort 3)
fromList (x:y:z:_:[])   = _Success # ThreePoints x y z
fromList (x:xs)         =  (:|) x <$> fromList xs

-- |
-- The expensive version of fromList that checks whether the head and last elements
-- are equal.
--
fromListWithEqCheck
    :: (Eq a, Validate v, Applicative (v (NonEmpty (ListToLinearRingError a))))
    => [a]
    -> v (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromListWithEqCheck xs = checkHeadAndLastEq xs *> fromList xs

-- |
-- Creates a LinearRing
-- @makeLinearRing xs x y z@ creates a `LinearRing` homomorphic to the list @xs ++ [x, y, z]@
--
makeLinearRing
    :: [a]          -- ^ The elements beyond the requisite 3 (which will go on the end)
    -> a            -- ^ Third Last Element
    -> a            -- ^ Second Last Element
    -> a            -- ^ Last Element
    -> LinearRing a
makeLinearRing ws x y z = foldr (:|) (ThreePoints x y z) ws


-- instances

instance (Show a) => Show (ListToLinearRingError a) where
    show (ListTooShort n) = "List too short: (length = " ++ show n ++ ")"
    show (HeadNotEqualToLast h l) = "head (" ++ show h ++ ") /= last(" ++ show l ++ ")"

instance (Show a) => Show (LinearRing a) where
    show  = show . fromLinearRing

instance Functor LinearRing where
    fmap f (ThreePoints x y z)  = ThreePoints (f x) (f y) (f z)
    fmap f (x :| xs)            = f x :| fmap f xs

-- | This instance of Foldable will run through the entire ring, closing the
-- loop by also passing the initial element in again at the end.
--
instance Foldable LinearRing where
--  foldr :: (a -> b -> b) -> b -> LinearRing a -> b
    foldr f x = foldr f x . fromLinearRing

-- |
-- When traversing this Structure, the Applicative context
-- of the last element will be appended to the end to close the loop
--
instance Traversable LinearRing where
--  traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
    traverse f xs = traverse' f xs <* f (ringHead xs)

instance (Eq a) => Eq (LinearRing a) where
    (==) = (==) `on` fromLinearRing

instance (ToJSON a) => ToJSON (LinearRing a) where
--  toJSON :: a -> Value
    toJSON = toJSON . fromLinearRing

instance (FromJSON a, Show a) => FromJSON (LinearRing a) where
--  parseJSON :: Value -> Parser a
    parseJSON v = do
        xs <- parseJSON v
        let vxs = fromListAcc xs
        maybe (parseError v (vxs ^? _Failure)) return (vxs ^? _Success)

-- helpers

fromListAcc :: [a] -> AccValidation (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromListAcc = fromList

-- |
-- This is a fold helper function and not a Foldable instance since it does not close the loop,
-- the first element is not replicated at the end.
--
-- > (\xs -> (ringHead xs) == (foldr'' (\a -> const a) 0 xs)) (xs :: LinearRing Int)
--
foldr'' :: (a -> b -> b) -> b -> LinearRing a -> b
foldr'' op e (ThreePoints x y z) = op x $ op y $ op z e
foldr'' op e (x :| xs) = op x (foldr'' op e xs)

-- |
--
-- This traverse function is a helper for the real traverse function, it doesnt close off the loop.
--
traverse' :: (Applicative f) => (a -> f b) -> LinearRing a -> f (LinearRing b)
traverse' f (ThreePoints x y z) = ThreePoints <$> f x <*> f y <*> f z
traverse' f (x :| xs) = (:|) <$> f x <*> traverse' f xs

showErrors :: (Show a) => NonEmpty (ListToLinearRingError a) -> String
showErrors = foldr (++) "" . intersperse ", " . fmap show

parseError :: (Show a) => Value -> Maybe (NonEmpty (ListToLinearRingError a)) -> Parser b
parseError v = maybe mzero (\e -> typeMismatch (showErrors e) v)

checkHeadAndLastEq
    :: (Eq a, Validate v, Functor (v (NonEmpty (ListToLinearRingError a))))
    => [a]
    -> v (NonEmpty (ListToLinearRingError a)) ()
checkHeadAndLastEq = maybe (_Failure # return (ListTooShort 0)) (\(h, l) -> if h == l then _Success # () else _Failure # return (HeadNotEqualToLast h l)) . mhl
    where
        mhl ::[a] -> Maybe (a, a)
        mhl xs = (,) <$> safeHead xs <*> safeLast xs

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs
