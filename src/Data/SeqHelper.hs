-------------------------------------------------------------------

-- |
-- Module       : Data.Geospatial.Internal.SeqHelpers
-- Copyright    : (C) 2014-2021 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
module Data.SeqHelper
  ( sequenceHead,
    sequenceTail,
    removeNextDuplicate,
  )
where

import qualified Data.Sequence as Sequence

-- All but the last
sequenceHead :: Sequence.Seq a -> Sequence.Seq a
sequenceHead (headS Sequence.:|> _) = headS
sequenceHead _ = Sequence.empty
{-# INLINE sequenceHead #-}

-- All but the first
sequenceTail :: Sequence.Seq a -> Sequence.Seq a
sequenceTail (_ Sequence.:<| tailS) = tailS
sequenceTail _ = Sequence.empty
{-# INLINE sequenceTail #-}

removeNextDuplicate :: (Eq a) => Sequence.Seq a -> Sequence.Seq a
removeNextDuplicate Sequence.Empty = Sequence.empty
removeNextDuplicate xs@(_ Sequence.:<| Sequence.Empty) = xs
removeNextDuplicate (x Sequence.:<| tailXs@(y Sequence.:<| _))
  | x /= y = x Sequence.<| removeNextDuplicate tailXs
  | otherwise = removeNextDuplicate tailXs
