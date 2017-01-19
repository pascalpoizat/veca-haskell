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
                     )
where

{- |
Minimal definition of 'complementary'.

You should ensure:

@
'complementary' . 'complementary' ≡ 'id'
isComplementary x y ≡ y == (complementary x)
@
-}
class Complementary p where
  complementary :: p a -> p a
  isComplementary :: (Eq (p a)) => (p a) -> (p a) -> Bool
  isComplementary = (==) . complementary
