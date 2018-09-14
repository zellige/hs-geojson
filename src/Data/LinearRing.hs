{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------
-- |
-- Module       : Data.LinearRing
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
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
    ,   VectorToLinearRingError(..)
    -- * Functions
    ,   toVector
    ,   combineToVector
    ,   fromVector
    ,   fromLinearRing
    ,   fromList
    ,   fromListWithEqCheck
    ,   makeLinearRing
    ,   ringHead
    ,   ringLength
    ) where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
import           Prelude             hiding (foldr)
#else
import           Prelude
#endif

import           Control.Applicative (Applicative (..))
import           Control.Lens        (( # ), (^?))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value)
import           Data.Aeson.Types    (Parser, typeMismatch)
import           Data.Foldable       (Foldable (..))
import           Data.Functor        ((<$>))
import           Data.List           (intercalate)
import           Data.List.NonEmpty  as NL (NonEmpty, toList)
import           Data.Traversable    (Traversable (..))
import qualified Data.Validation     as Validation
import qualified Data.Vector         as Vector

-- |
-- a LinearRing has at least 3 (distinct) elements
--
data LinearRing a = LinearRing a a a (Vector.Vector a) deriving (Eq, Show)

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
    deriving (Eq)

-- |
-- When converting a Vector to a LinearRing there are some things that can go wrong
--
--     * The vector can be too short
--     * The head may not be equal to the last element in the list
--
data VectorToLinearRingError a =
    VectorTooShort Int
  | FirstNotEqualToLast a a
  deriving (Eq)

-- functions

-- |
-- returns the element at the head of the ring
--
ringHead :: LinearRing a -> a
ringHead (LinearRing x _ _ _)   = x

-- |
-- returns the number of elements in the list, including the replicated element at the end of the list.
--
ringLength :: LinearRing a -> Int
ringLength (LinearRing _ _ _ xs) = 4 + Vector.length xs

-- |
-- This function converts it into a list and appends the given element to the end.
--
fromLinearRing :: LinearRing a -> [a]
fromLinearRing (LinearRing x y z ws) = x : y : z : Vector.foldr (:) [x] ws

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
-- Unfortunately it doesn't check that the last element is the same as the first at the moment...
--
fromList :: (Eq a, Show a, Validation.Validate v, Functor (v (NonEmpty (ListToLinearRingError a)))) => [a] -> v (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromList (x:y:z:ws@(_:_)) = Validation._Success # LinearRing x y z (fromListDropLast ws)
fromList xs               = Validation._Failure # pure (ListTooShort (length xs))

-- |
-- The expensive version of fromList that checks whether the head and last elements
-- are equal.
--
fromListWithEqCheck :: (Eq a, Show a, Validation.Validate v, Applicative (v (NonEmpty (ListToLinearRingError a)))) => [a] -> v (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromListWithEqCheck xs = checkHeadAndLastEq xs *> fromList xs

-- |
-- create a vector from a LinearRing by combining values.
-- LineString 1 2 3 [4,1] (,) --> Vector [(1,2),(2,3),(3,4),(4,1)]
--
combineToVector :: (a -> a -> b) -> LinearRing a -> Vector.Vector b
combineToVector combine (LinearRing a b c rest) = Vector.cons (combine a b) (Vector.cons (combine b c) combineRest)
    where
        combineRest =
          if Vector.null rest
            then
              Vector.empty
            else
              (Vector.zipWith combine <*> Vector.tail) (Vector.cons c rest)

-- |
-- create a vector from a LinearRing.
-- LineString 1 2 3 [4,1] --> Vector [1,2,3,4,1)]
--
toVector :: LinearRing a -> Vector.Vector a
toVector (LinearRing a b c rest) = Vector.cons a (Vector.cons b (Vector.cons c rest))

-- |
-- creates a LinearRing out of a vector of elements,
-- if there are enough elements (needs at least 3) elements
--
-- fromVector (x:y:z:ws@(_:_)) = _Success # LinearRing x y z (fromListDropLast ws)
-- fromList xs               = _Failure # return (ListTooShort (length xs))

fromVector :: (Eq a, Show a, Validation.Validate v, Functor (v (NonEmpty (ListToLinearRingError a)))) => Vector.Vector a -> v (NonEmpty (VectorToLinearRingError a)) (LinearRing a)
fromVector v =
  if Vector.length v >= 3 then
    if Vector.head v == Vector.last v then
        Validation._Success # LinearRing (Vector.unsafeIndex v 0) (Vector.unsafeIndex v 1) (Vector.unsafeIndex v 2) (Vector.drop 3 v)
    else
        Validation._Failure # pure (FirstNotEqualToLast (Vector.head v) (Vector.last v))
  else
    Validation._Failure # pure (VectorTooShort (Vector.length v))

-- |
-- Creates a LinearRing
-- @makeLinearRing x y z xs@ creates a `LinearRing` homomorphic to the list @[x, y, z] ++ xs@
-- the list @xs@ should NOT contain the first element repeated, i.e the loop does not need to
-- be closed, makeLinearRing will close it off.
--
-- Repeating the first element is just redundant.
--
makeLinearRing :: (Eq a, Show a) =>
       a                -- ^ The first element
    -> a                -- ^ The second element
    -> a                -- ^ The third element
    -> Vector.Vector a  -- ^ The rest of the optional elements (WITHOUT the first element repeated at the end)
    -> LinearRing a
makeLinearRing = LinearRing

-- instances

instance (Show a) => Show (ListToLinearRingError a) where
    show (ListTooShort n) = "List too short: (length = " ++ show n ++ ")"
    show (HeadNotEqualToLast h l) = "head (" ++ show h ++ ") /= last(" ++ show l ++ ")"

instance (Show a) => Show (VectorToLinearRingError a) where
    show (VectorTooShort n) = "Vector too short: (length = " ++ show n ++ ")"
    show (FirstNotEqualToLast h l) = "head (" ++ show h ++ ") /= last(" ++ show l ++ ")"

instance Functor LinearRing where
     fmap f (LinearRing x y z ws) = LinearRing (f x) (f y) (f z) (Vector.map f ws)

-- | This instance of Foldable will run through the entire ring, closing the
-- loop by also passing the initial element in again at the end.
--
instance Foldable LinearRing where
--  foldr :: (a -> b -> b) -> b -> LinearRing a -> b
    foldr f u (LinearRing x y z ws) = f x (f y (f z (Vector.foldr f (f x u) ws)))

-- |
-- When traversing this Structure, the Applicative context
-- of the last element will be appended to the end to close the loop
--
instance Traversable LinearRing where
-- --  sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
    sequenceA (LinearRing fx fy fz fws) = (LinearRing <$> fx <*> fy <*> fz <*> sequenceA fws) <* fx

instance ToJSON a => ToJSON (LinearRing a) where
--  toJSON :: a -> Value
    toJSON = toJSON . fromLinearRing

instance (Eq a, FromJSON a, Show a) => FromJSON (LinearRing a) where
--  parseJSON :: Value -> Parser a
    parseJSON v = do
        xs <- parseJSON v
        let vxs = fromListAcc xs
        maybe (parseError v (vxs ^? Validation._Failure)) return (vxs ^? Validation._Success)

-- helpers

fromListAcc :: (Eq a, Show a) => [a] -> Validation.Validation (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromListAcc = fromList

showErrors :: (Show a) => NonEmpty (ListToLinearRingError a) -> String
showErrors = intercalate ", " . NL.toList . fmap show

parseError :: (Show a) => Value -> Maybe (NonEmpty (ListToLinearRingError a)) -> Parser b
parseError v = maybe mzero (\e -> typeMismatch (showErrors e) v)

checkHeadAndLastEq
    :: (Eq a, Validation.Validate v, Functor (v (NonEmpty (ListToLinearRingError a))))
    => [a]
    -> v (NonEmpty (ListToLinearRingError a)) ()
checkHeadAndLastEq = maybe (Validation._Failure # pure (ListTooShort 0)) (\(h, l) -> if h == l then Validation._Success # () else Validation._Failure # pure (HeadNotEqualToLast h l)) . mhl
    where
        mhl ::[a] -> Maybe (a, a)
        mhl xs = (,) <$> safeHead xs <*> safeLast xs

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

fromListDropLast :: Eq a => [a] -> Vector.Vector a
fromListDropLast []  = Vector.empty
fromListDropLast [_] = Vector.empty
fromListDropLast x   = Vector.unsafeInit $ Vector.fromList x
