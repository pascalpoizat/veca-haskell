-- module for Trifunctors
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
--
-- this module is largely inspired by Data.Bifunctor
module Trifunctor ( Trifunctor(..)
                  )
where

class Trifunctor p  where
  trimap
    :: (a -> a') -> (b -> b') -> (c -> c') -> p a b c -> p a' b' c'
  trimap f g h = first f . second g . third h
  first :: (a -> a') -> p a b c -> p a' b c
  first f = trimap f id id
  second :: (b -> b') -> p a b c -> p a b' c
  second g = trimap id g id
  third :: (c -> c') -> p a b c -> p a b c'
  third h = trimap id id h

instance Trifunctor (,,) where
  trimap f g h ~(a, b, c) = (f a, g b, h c)
  {-# INLINE trimap #-}

instance Trifunctor ((,,,) x) where
  trimap f g h ~(x, a, b, c) = (x, f a, g b, h c)
  {-# INLINE trimap #-}

instance Trifunctor ((,,,,) x y) where
  trimap f g h ~(x, y, a, b, c) = (x, y, f a, g b, h c)
  {-# INLINE trimap #-}

instance Trifunctor ((,,,,,) x y z) where
  trimap f g h ~(x, y, z, a, b, c) = (x, y, z, f a, g b, h c)
  {-# INLINE trimap #-}

-- we do not define instance Trifunctor Either/Const/Tagged because we have 3 items, not 2
-- and we do not know how to deal with this
