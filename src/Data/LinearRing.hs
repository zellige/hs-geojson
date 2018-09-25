{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
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
    ,   Data.LinearRing.map
    ,   Data.LinearRing.foldr
    ,   Data.LinearRing.foldMap
    ,   ringHead
    ,   ringLength
    ) where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
import           Prelude             hiding (foldr)
#else
import           Prelude
#endif

import           Control.Applicative (Applicative (..))
import           Control.DeepSeq
import           Control.Lens        (( # ), (^?))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value)
import           Data.Aeson.Types    (Parser, typeMismatch)
import qualified Data.Foldable       as Foldable
import           Data.Functor        ((<$>))
import           Data.List           (intercalate)
import           Data.List.NonEmpty  as NL (NonEmpty, toList)
import qualified Data.Sequence       as Sequence
import qualified Data.Validation     as Validation
import           GHC.Generics        (Generic)

-- |
-- a LinearRing has at least 3 (distinct) elements
--
data LinearRing a = LinearRing a a a (Sequence.Seq a) deriving (Eq, Show, Generic, NFData)

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
ringLength (LinearRing _ _ _ xs) = 4 + Sequence.length xs

-- |
-- This function converts it into a list and appends the given element to the end.
--
fromLinearRing :: LinearRing a -> [a]
fromLinearRing (LinearRing x y z ws) = x : y : z : Foldable.foldr (:) [x] ws

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
{-# INLINE fromList #-}

-- |
-- The expensive version of fromList that checks whether the head and last elements
-- are equal.
--
fromListWithEqCheck :: (Eq a, Show a, Validation.Validate v, Applicative (v (NonEmpty (ListToLinearRingError a)))) => [a] -> v (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromListWithEqCheck xs = checkHeadAndLastEq xs *> fromList xs

-- |
-- create a vector from a LinearRing by combining values.
-- LinearRing 1 2 3 [4,1] (,) --> Vector [(1,2),(2,3),(3,4),(4,1)]
--
combineToVector :: (a -> a -> b) -> LinearRing a -> Sequence.Seq b
combineToVector combine (LinearRing a b c rest) = combine a b Sequence.:<| (combine b c Sequence.:<| combineRest)
    where
        combineRest =
          if Sequence.null rest
            then
              Sequence.empty
            else
                (Sequence.zipWith combine <*> sequenceTail) (b Sequence.<| rest)
{-# INLINE combineToVector #-}

sequenceTail :: Sequence.Seq a -> Sequence.Seq a
sequenceTail (head Sequence.:<| tail) = tail

-- |
-- create a vector from a LinearRing.
-- LinearRing 1 2 3 [4,1] --> Vector [1,2,3,4,1)]
--
toVector :: LinearRing a -> Sequence.Seq a
toVector (LinearRing a b c rest) = a Sequence.:<| (b Sequence.:<| (c Sequence.:<| rest))
{-# INLINE toVector #-}

-- |
-- creates a LinearRing out of a vector of elements,
-- if there are enough elements (needs at least 3) elements
--
-- fromVector (x:y:z:ws@(_:_)) = _Success # LinearRing x y z (fromListDropLast ws)
-- fromList xs               = _Failure # return (ListTooShort (length xs))

fromVector :: (Eq a, Show a, Validation.Validate v, Functor (v (NonEmpty (ListToLinearRingError a)))) => Sequence.Seq a -> v (NonEmpty (VectorToLinearRingError a)) (LinearRing a)
fromVector v@(head Sequence.:<| tail@(_ Sequence.:|> last)) =
  if Sequence.length v >= 3 then
    if head == last then
        Validation._Success # LinearRing (Sequence.index v 0) (Sequence.index v 1) (Sequence.index v 2) (Sequence.drop 3 v)
    else
        Validation._Failure # pure (FirstNotEqualToLast head last)
  else
    Validation._Failure # pure (VectorTooShort (Sequence.length v))
{-# INLINE fromVector #-}

-- |
-- Creates a LinearRing
-- @makeLinearRing x y z xs@ creates a `LinearRing` homomorphic to the list @[x, y, z] ++ xs@
-- the list @xs@ should NOT contain the first element repeated, i.e the loop does not need to
-- be closed, makeLinearRing will close it off.
--
-- Repeating the first element is just redundant.
--
makeLinearRing :: (Eq a, Show a) =>
       a                        -- ^ The first element
    -> a                        -- ^ The second element
    -> a                        -- ^ The third element
    -> Sequence.Seq a  -- ^ The rest of the optional elements (WITHOUT the first element repeated at the end)
    -> LinearRing a
makeLinearRing = LinearRing

-- instances

instance (Show a) => Show (ListToLinearRingError a) where
    show (ListTooShort n) = "List too short: (length = " ++ show n ++ ")"
    show (HeadNotEqualToLast h l) = "head (" ++ show h ++ ") /= last(" ++ show l ++ ")"

instance (Show a) => Show (VectorToLinearRingError a) where
    show (VectorTooShort n) = "Vector too short: (length = " ++ show n ++ ")"
    show (FirstNotEqualToLast h l) = "head (" ++ show h ++ ") /= last(" ++ show l ++ ")"

map :: (a -> b) -> LinearRing a -> LinearRing b
map f (LinearRing x y z ws) = LinearRing (f x) (f y) (f z) (fmap f ws)
{-# INLINE map #-}

-- | This will run through the entire ring, closing the
-- loop by also passing the initial element in again at the end.
--
foldr :: (a -> b -> b) -> b -> LinearRing a -> b
foldr f u (LinearRing x y z ws) = f x (f y (f z (Foldable.foldr f (f x u) ws)))
{-# INLINE foldr #-}

foldMap :: (Monoid m) => (a -> m) -> LinearRing a -> m
foldMap f = foldr (mappend . f) mempty
{-# INLINE foldMap #-}

instance (ToJSON a) => ToJSON (LinearRing a) where
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

checkHeadAndLastEq :: (Eq a, Validation.Validate v, Functor (v (NonEmpty (ListToLinearRingError a))))
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

fromListDropLast :: (Eq a) => [a] -> Sequence.Seq a
fromListDropLast []  = Sequence.empty
fromListDropLast [_] = Sequence.empty
fromListDropLast x   = sequenceTail $ Sequence.fromList x
