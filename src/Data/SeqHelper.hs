-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.SeqHelpers
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- see Section 2.1.1 /Position/ in the GeoJSON Spec
-------------------------------------------------------------------
module Data.SeqHelper (
  sequenceHead
  , sequenceTail
  ) where

import qualified Data.Sequence as Sequence

-- All but the last
sequenceHead :: Sequence.Seq a -> Sequence.Seq a
sequenceHead (headS Sequence.:|> _) = headS
sequenceHead _                      = Sequence.empty
{-# INLINE sequenceHead #-}

-- All but the first
sequenceTail :: Sequence.Seq a -> Sequence.Seq a
sequenceTail (_ Sequence.:<| tailS) = tailS
sequenceTail _                      = Sequence.empty
{-# INLINE sequenceTail #-}
