-----------------------------------------------------------------------------
-- |
-- Module      :  Models.Complementary
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A class for events with a complementarity relation.
-----------------------------------------------------------------------------

module Models.Complementary (
  Complementary(..)
  , isComplementary)
where

{- |
A typeclass for types (often events) with a complementary relation.

Minimal definition: 'complementary'.

You should ensure:

@
'complementary' . 'complementary' ≡ 'id'
@
-}
class Complementary p  where
  {-# MINIMAL complementary #-}
  complementary :: p -> p

-- |Check if two things are complementary.
isComplementary :: (Complementary p
                   ,Eq p)
                => p -> p -> Bool
isComplementary = (==) . complementary
