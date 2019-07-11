{-|
Module      : Helpers
Description : Helper functions.
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Helpers
  ( contains
  , allIn
  , fixpoint
  , fixpoint'
  , removeDuplicates
  , forall
  , exists
  , exists1
  )
where

import           Data.Foldable                  ( toList )
import           Data.List                      ( nub )
import           Data.Monoid                    ( All(..)
                                                , Any(..)
                                                )
import           Data.Set                       ( fromList )

{-|
Check if a foldable contains an element.

Dual of elem in Data.Foldable.

@xs contains x = x elem xs@
-}
contains :: (Foldable t, Eq a) => t a -> a -> Bool
contains = flip elem

{-|
Check if all elements of a foldable are included in another one.

@xs allIn ys = forall x in xs . x in ys@
-}
allIn :: (Foldable t, Eq a) => t a -> t a -> Bool
allIn xs ys = getAll $ foldMap (All . contains ys) xs

{-|
Check if all elements of a list verify some predicate.
-}
forall :: [a] -> (a -> Bool) -> Bool
forall ss p = getAll . mconcat $ All . p <$> ss

{-|
Check if some elements of a list verify some predicate.
-}
exists :: [a] -> (a -> Bool) -> Bool
exists ss p = getAny . mconcat $ Any . p <$> ss

{-|
Check if exactly one element of a list verifies some predicate.
-}
exists1 :: [a] -> (a -> Bool) -> Bool
exists1 ss p = length (filter p ss) == 1

{-|
Fixpoint (strict).

Stops upon strict equality, i.e. does not stop if @f [1,2] = [2,1]@
-}
fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x | x == x'   = x
             | otherwise = fixpoint f x'
  where x' = f x

{-|
Fixpoint (based on sets).

Stops upon set equality, i.e. will stop if @f [1,2] = [2,1]@
-}
fixpoint' :: (Foldable t, Ord a) => (t a -> t a) -> t a -> t a
fixpoint' f xs | (fromList . toList) xs == (fromList . toList) xs' = xs
               | otherwise = fixpoint' f xs'
  where xs' = f xs

{-|
Remove duplicates from a list.
-}
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = nub
