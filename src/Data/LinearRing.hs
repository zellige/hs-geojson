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
    ,   ListToLinearRingError(..)
    -- * Functions
    ,   fromLinearRing
    ,   fromList
    ,   fromListWithEqCheck
    ,   makeLinearRing
    ,   ringHead
    ,   ringLength
    ) where

import Prelude hiding ( foldr )

import Control.Applicative ( Applicative(..) )
import Control.Lens ( ( # ), (^?) )
import Control.Monad ( mzero )
import Data.Aeson ( ToJSON(..), FromJSON(..), Value )
import Data.Aeson.Types ( Parser, typeMismatch )
import Data.Foldable ( Foldable(..) )
import Data.Functor ( (<$>) )
import Data.List ( intercalate )
import Data.List.NonEmpty as NL ( NonEmpty, toList )
import Data.Traversable ( Traversable(..) )
import Data.Validation ( Validate(..), AccValidation, _Failure, _Success )

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
data LinearRing a = LinearRing a a a [a] deriving (Eq)

-- |
-- When converting a List to a LinearRing there are some things that can go wrong
--
--     * The list can be too short
--     * The head may not be equal to the last element in the list (NB this is not currently checked due to performance concerns,
--       and it also doesnt make much sense since its likely to contain doubles)
--
data ListToLinearRingError a =
        ListTooShort Int
    |   HeadNotEqualToLast a a

-- functions

-- |
-- returns the element at the head of the ring
--
ringHead :: LinearRing a -> a
ringHead (LinearRing x _ _ _)   = x

-- NOTE (Dom De Re): Props have been commented out until <https://github.com/sol/doctest-haskell/issues/83>
-- has been resolved.

-- |
-- returns the number of elements in the list, including the replicated element at the end of the list.
--
-- prop> (\xs -> ringLength xs == (length (fromLinearRing xs))) (xs :: LinearRing Int)
--
ringLength :: LinearRing a -> Int
ringLength (LinearRing _ _ _ xs) = 4 + length xs

-- |
-- This function converts it into a list and appends the given element to the end.
--
-- (\xs -> safeLast (fromLinearRing xs) == Just (ringHead xs)) (xs :: LinearRing Int)
--
-- (\xs -> length (fromLinearRing xs) >= 4) (xs :: LinearRing Int)
--

fromLinearRing :: LinearRing a -> [a]
fromLinearRing (LinearRing x y z ws) = x : y : z : foldr (:) [x] ws

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
-- >>> fromList [] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccFailure (List too short: (length = 0) :| [])
--
-- >>> fromList [0] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccFailure (List too short: (length = 1) :| [])
--
-- >>> fromList [0, 1] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccFailure (List too short: (length = 2) :| [])
--
-- >>> fromList [0, 1, 2] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccFailure (List too short: (length = 3) :| [])
--
-- >>> fromList [0, 1, 2, 3] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccSuccess [0,1,2,0]
--
-- >>> fromList [0, 1, 2, 4, 0] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccSuccess [0,1,2,4,0]
--
-- >>> fromList [0, 1, 2, 4, 5, 0] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccSuccess [0,1,2,4,5,0]
--
-- Unfortunately it doesn't check that the last element is the same as the first at the moment...
--
-- >>> fromList [0, 1, 2, 4, 5, 6] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccSuccess [0,1,2,4,5,0]
--
fromList
    :: (Validate v, Functor (v (NonEmpty (ListToLinearRingError a))))
    => [a]
    -> v (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromList (x:y:z:ws@(_:_))   = _Success # LinearRing x y z (foldrDropLast (:) [] ws)
fromList xs                 = _Failure # return (ListTooShort (length xs))

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
-- @makeLinearRing x y z xs@ creates a `LinearRing` homomorphic to the list @[x, y, z] ++ xs@
-- the list @xs@ should NOT contain the first element repeated, i.e the loop does not need to
-- be closed, makeLinearRing will close it off.
--
-- Repeating the first element is just redundant.
--
makeLinearRing
    :: a            -- ^ The first element
    -> a            -- ^ The second element
    -> a            -- ^ The third element
    -> [a]          -- ^ The rest of the optional elements (WITHOUT the first element repeated at the end)
    -> LinearRing a
makeLinearRing = LinearRing


-- instances

instance (Show a) => Show (ListToLinearRingError a) where
    show (ListTooShort n) = "List too short: (length = " ++ show n ++ ")"
    show (HeadNotEqualToLast h l) = "head (" ++ show h ++ ") /= last(" ++ show l ++ ")"

instance (Show a) => Show (LinearRing a) where
    show  = show . fromLinearRing

instance Functor LinearRing where
    fmap f (LinearRing x y z ws) = LinearRing (f x) (f y) (f z) (fmap f ws)

-- | This instance of Foldable will run through the entire ring, closing the
-- loop by also passing the initial element in again at the end.
--
-- > (\xs -> (foldr (:) [] xs) == (fromLinearRing xs)) (xs :: LinearRing Int)
--
-- > (\xs -> (ringHead xs) == (foldr'' (\a -> const a) 0 xs)) (xs :: LinearRing Int)
--
instance Foldable LinearRing where
--  foldr :: (a -> b -> b) -> b -> LinearRing a -> b
    foldr f u (LinearRing x y z ws) = f x (f y (f z (foldr f (f x u) ws)))

-- |
-- When traversing this Structure, the Applicative context
-- of the last element will be appended to the end to close the loop
--
instance Traversable LinearRing where
--  sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
    sequenceA (LinearRing fx fy fz fws) = (LinearRing <$> fx <*> fy <*> fz <*> sequenceA fws) <* fx

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

showErrors :: (Show a) => NonEmpty (ListToLinearRingError a) -> String
showErrors = intercalate ", " . NL.toList . fmap show

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
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

-- |
-- Does a fold but ignores the last element of the list
--
-- > (\x xs -> length (foldrDropLast (:) [] (x : xs)) == length xs) (x :: Int) (xs :: [Int])
--
foldrDropLast :: (a -> b -> b) -> b -> [a] -> b
foldrDropLast _ x []     = x
foldrDropLast _ x [_]    = x
foldrDropLast f x (y:ys) = f y (foldrDropLast f x ys)
