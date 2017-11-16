{-|
Module      : Transformations.Substitution
Description : Substitutions (from a to a)
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}

module Transformations.Substitution (Substitution
                                    ,empty
                                    ,apply
                                    ,boundvariables
                                    ,freevariables)

where

import           Data.Map (findWithDefault, fromList, keys)

{-|
A substitution from a to a
-}
type Substitution a = [(a,a)]

{-|
Empty substitution.
-}
empty :: Substitution a
empty = []

{-|
Apply a substitution.
-}
apply :: Ord a => Substitution a -> a -> a
apply sub a = findWithDefault a a $ fromList sub

{-|
Variables bound by a substitution.
-}
boundvariables :: Ord a => Substitution a -> [a]
boundvariables = keys . fromList

{-|
Variables of a list free wrt a substitution.
-}
freevariables :: (Eq a, Ord a) => Substitution a -> [a] -> [a]
freevariables sub = filter (`notElem` boundvariables sub)
