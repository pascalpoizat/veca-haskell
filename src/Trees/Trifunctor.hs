-----------------------------------------------------------------------------
-- |
-- Module      :  Trees.Trifunctor
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A class for trifunctors largely inspired by @Data.Bifunctor@.
-----------------------------------------------------------------------------

module Trees.Trifunctor (
  Trifunctor(..))
where

{- |
Minimal definition: 'trimap' or 'first', 'second', and 'third'.

Formally, the class 'Trifunctor' represents a trifunctor
from @Hask@ -> @Hask@.

Intuitively it is a trifunctor where first, second, and third argument are covariant.

You can define a 'Trifunctor' by either defining 'trimap' or
by defining all three 'first', 'second', and 'third'.

If you supply 'trimap', you should ensure:

@'trimap' 'id' 'id' 'id' ≡ 'id'@

If you supply 'first', 'second', and 'third', ensure:

@
'first' 'id' ≡ 'id'
'second' 'id' ≡ 'id'
'third' 'id' ≡ 'id'
@

If you supply both, you should also ensure:

@'trimap' f g h ≡ 'first' f '.' 'second' g '.' 'third' h@

These ensure by parametricity:

@
'trimap'  (f1 '.' f2) (g1 '.' g2) (h1 '.' h2) ≡ 'trimap' f1 g1 h1 '.' 'trimap' f2 g2 h2
'first' (f1 '.' f2) ≡ 'first' f1 '.' 'first' 'f2'
'second' (g1 '.' g2) ≡ 'second' g1 '.' 'second' 'g2'
'third' (h1 '.' h2) ≡ 'third' h1 '.' 'third' 'h2'
@
-}
class Trifunctor p  where
  -- |Map over the three arguments at the same time
  --
  -- @'trimap' f g h ≡ 'first' f '.' 'second' g '.' 'third' h@
  trimap
    :: (a -> a') -> (b -> b') -> (c -> c') -> p a b c -> p a' b' c'
  trimap f g h = first f . second g . third h
  -- |Map covariantly over the first argument
  --
  -- @'first' f ≡ 'trimap' f 'id' 'id'@
  first :: (a -> a') -> p a b c -> p a' b c
  first f = trimap f id id
  -- |Map covariantly over the second argument
  --
  -- @'second' g ≡ 'trimap' 'id' g 'id'@
  second :: (b -> b') -> p a b c -> p a b' c
  second g = trimap id g id
  -- |Map covariantly over the third argument
  --
  -- @'third' h ≡ 'trimap' 'id' 'id' h@
  third :: (c -> c') -> p a b c -> p a b c'
  third = trimap id id
  {-# MINIMAL trimap | first , second , third #-}

instance Trifunctor (,,) where
  trimap f g h ~(a,b,c) = (f a,g b,h c)
  {-# INLINE trimap #-}

instance Trifunctor ((,,,) x) where
  trimap f g h ~(x,a,b,c) = (x,f a,g b,h c)
  {-# INLINE trimap #-}

instance Trifunctor ((,,,,) x y) where
  trimap f g h ~(x,y,a,b,c) = (x,y,f a,g b,h c)
  {-# INLINE trimap #-}

instance Trifunctor ((,,,,,) x y z) where
  trimap f g h ~(x,y,z,a,b,c) = (x,y,z,f a,g b,h c)
  {-# INLINE trimap #-}
