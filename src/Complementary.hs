-----------------------------------------------------------------------------
-- |
-- Module      :  Complementary
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A class for events with a complementarity relation.
-----------------------------------------------------------------------------

module Complementary ( Complementary(..)
                     , isComplementary
                     )
where

{- |
Minimal definition: 'complementary'.

You should ensure:

@
'complementary' . 'complementary' ≡ 'id'
isComplementary x y ≡ y == (complementary x)
@
-}
class Complementary p where
  {-# MINIMAL complementary #-}
  complementary :: p -> p

isComplementary :: (Complementary p, Eq p) => p -> p -> Bool
isComplementary = (==) . complementary
