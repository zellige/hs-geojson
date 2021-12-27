{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------

-- |
-- Module       : Data.LinearRing
-- Copyright    : (C) 2014-2021 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- Refer to the GeoJSON Spec <http://geojson.org/geojson-spec.html#polygon>
--
-- A LinearRing is a List with at least 4 elements, where the
-- first element is expected to be the same as the last.
module Data.LinearRing
  ( -- * Type
    LinearRing,
    ListToLinearRingError (..),
    SequenceToLinearRingError (..),

    -- * Functions
    toSeq,
    combineToSeq,
    fromSeq,
    fromLinearRing,
    fromList,
    fromListWithEqCheck,
    makeLinearRing,
    ringHead,
    ringLength,
  )
where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
import           Prelude             hiding (foldr)
#else
import           Prelude
#endif

import Control.DeepSeq
import Control.Lens ((#), (^?))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Foldable as Foldable
import Data.List (intercalate)
import Data.List.NonEmpty as NL (NonEmpty, toList)
import qualified Data.SeqHelper as SeqHelper
import qualified Data.Sequence as Sequence
import qualified Data.Validation as Validation
import GHC.Generics (Generic)

-- |
-- a LinearRing has at least 3 (distinct) elements
data LinearRing a = LinearRing a a a (Sequence.Seq a) deriving (Eq, Show, Generic, NFData)

-- |
-- When converting a List to a LinearRing there are some things that can go wrong
--
--     * The list can be too short
--     * The head may not be equal to the last element in the list (NB this is not currently checked due to performance concerns,
--       and it also doesnt make much sense since its likely to contain doubles)
data ListToLinearRingError a
  = ListTooShort Int
  | HeadNotEqualToLast a a
  deriving (Eq)

-- |
-- When converting a Sequence to a LinearRing there are some things that can go wrong
--
--     * The sequence can be too short
--     * The head may not be equal to the last element in the list
data SequenceToLinearRingError a
  = SequenceTooShort Int
  | FirstNotEqualToLast a a
  deriving (Eq)

-- functions

-- |
-- returns the element at the head of the ring
ringHead :: LinearRing a -> a
ringHead (LinearRing x _ _ _) = x

-- |
-- returns the number of elements in the list, including the replicated element at the end of the list.
ringLength :: LinearRing a -> Int
ringLength (LinearRing _ _ _ xs) = 4 + Sequence.length xs

-- |
-- This function converts it into a list and appends the given element to the end.
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
fromList :: (Eq a, Show a, Validation.Validate v, Functor (v (NonEmpty (ListToLinearRingError a)))) => [a] -> v (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromList (x : y : z : ws@(_ : _)) = Validation._Success # LinearRing x y z (fromListDropLast ws)
fromList xs = Validation._Failure # pure (ListTooShort (length xs))
{-# INLINE fromList #-}

-- |
-- The expensive version of fromList that checks whether the head and last elements
-- are equal.
fromListWithEqCheck :: (Eq a, Show a, Validation.Validate v, Applicative (v (NonEmpty (ListToLinearRingError a)))) => [a] -> v (NonEmpty (ListToLinearRingError a)) (LinearRing a)
fromListWithEqCheck xs = checkHeadAndLastEq xs *> fromList xs

-- |
-- create a sequence from a LinearRing by combining values.
-- LinearRing 1 2 3 [4,1] (,) --> Seq [(1,2),(2,3),(3,4),(4,1)]
combineToSeq :: (a -> a -> b) -> LinearRing a -> Sequence.Seq b
combineToSeq combine (LinearRing a b c rest) = combine a b Sequence.:<| (combine b c Sequence.:<| combineRest)
  where
    combineRest =
      if Sequence.null rest
        then Sequence.empty
        else (Sequence.zipWith combine <*> SeqHelper.sequenceTail) (c Sequence.<| rest)
{-# INLINE combineToSeq #-}

-- |
-- create a sequence from a LinearRing.
-- LinearRing 1 2 3 [4,1] --> Seq [1,2,3,4,1)]
toSeq :: LinearRing a -> Sequence.Seq a
toSeq (LinearRing a b c rest) = a Sequence.:<| (b Sequence.:<| (c Sequence.:<| rest))
{-# INLINE toSeq #-}

-- |
-- creates a LinearRing out of a sequence of elements,
-- if there are enough elements (needs at least 3) elements
--
-- fromSeq (x:y:z:ws@(_:_))  = _Success # LinearRing x y z (fromListDropLast ws)
-- fromSeq xs                = _Failure # return (ListTooShort (length xs))
fromSeq :: (Eq a, Show a, Validation.Validate v, Functor (v (NonEmpty (ListToLinearRingError a)))) => Sequence.Seq a -> v (NonEmpty (SequenceToLinearRingError a)) (LinearRing a)
fromSeq as =
  case as of
    (first Sequence.:<| (second Sequence.:<| (third Sequence.:<| rest@(_ Sequence.:|> lastS)))) ->
      if first == lastS
        then Validation._Success # LinearRing first second third rest
        else Validation._Failure # pure (FirstNotEqualToLast first lastS)
    (first Sequence.:<| (second Sequence.:<| (third Sequence.:<| _))) ->
      if first == third
        then Validation._Success # LinearRing first second third Sequence.empty
        else Validation._Failure # pure (FirstNotEqualToLast first third)
    v -> Validation._Failure # pure (SequenceTooShort (Sequence.length v))
{-# INLINE fromSeq #-}

-- |
-- Creates a LinearRing
-- @makeLinearRing x y z xs@ creates a `LinearRing` homomorphic to the list @[x, y, z] ++ xs@
-- the list @xs@ should NOT contain the first element repeated, i.e the loop does not need to
-- be closed, makeLinearRing will close it off.
--
-- Repeating the first element is just redundant.
makeLinearRing ::
  (Eq a, Show a) =>
  -- | The first element
  a ->
  -- | The second element
  a ->
  -- | The third element
  a ->
  -- | The rest of the optional elements (WITHOUT the first element repeated at the end)
  Sequence.Seq a ->
  LinearRing a
makeLinearRing = LinearRing

-- instances

instance (Show a) => Show (ListToLinearRingError a) where
  show (ListTooShort n) = "List too short: (length = " ++ show n ++ ")"
  show (HeadNotEqualToLast h l) = "head (" ++ show h ++ ") /= last(" ++ show l ++ ")"

instance (Show a) => Show (SequenceToLinearRingError a) where
  show (SequenceTooShort n) = "Sequence too short: (length = " ++ show n ++ ")"
  show (FirstNotEqualToLast h l) = "head (" ++ show h ++ ") /= last(" ++ show l ++ ")"

instance Functor LinearRing where
  fmap f (LinearRing x y z ws) = LinearRing (f x) (f y) (f z) (fmap f ws)

-- | This instance of Foldable will run through the entire ring, closing the
-- loop by also passing the initial element in again at the end.
instance Foldable LinearRing where
  --  foldr :: (a -> b -> b) -> b -> LinearRing a -> b
  foldr f u (LinearRing x y z ws) = f x (f y (f z (Foldable.foldr f (f x u) ws)))

-- |
-- When traversing this Structure, the Applicative context
-- of the last element will be appended to the end to close the loop
instance Traversable LinearRing where
  --  sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
  sequenceA (LinearRing fx fy fz fws) = (LinearRing <$> fx <*> fy <*> fz <*> sequenceA fws) <* fx

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

checkHeadAndLastEq ::
  (Eq a, Validation.Validate v, Functor (v (NonEmpty (ListToLinearRingError a)))) =>
  [a] ->
  v (NonEmpty (ListToLinearRingError a)) ()
checkHeadAndLastEq = maybe (Validation._Failure # pure (ListTooShort 0)) (\(h, l) -> if h == l then Validation._Success # () else Validation._Failure # pure (HeadNotEqualToLast h l)) . mhl
  where
    mhl :: [a] -> Maybe (a, a)
    mhl xs = (,) <$> safeHead xs <*> safeLast xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : xs) = safeLast xs

fromListDropLast :: (Eq a) => [a] -> Sequence.Seq a
fromListDropLast [] = Sequence.empty
fromListDropLast [_] = Sequence.empty
fromListDropLast x = SeqHelper.sequenceHead $ Sequence.fromList x
