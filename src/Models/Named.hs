{-|
Module      : Models.Named
Description : A class for string-named elements.
Copyright   : (c) 2018 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Models.Named (
  Named(..)
  )
where

import           Data.Monoid ((<>))
import           Models.Name

{- |
A class for string-named elements.

Minimal definition: getName, setName.
-}
class Named p where
  {-# MINIMAL name, rename #-}
  {-|
  Get the name of a named element.
  -}
  name :: p -> Name String
  {-|
  Rename a named element.
  -}
  rename :: Name String -> p -> p
  {-|
  Prefix the name of a named element by a name.

  prefixBy x a.b give x.a.b.
  -}
  prefixBy :: Name String -> p -> p
  prefixBy x p = rename (x <> name p) p
  {-|
  Suffix the name of a named element by a name.

  suffixBy x a.b gives a.b.x.
  -}
  suffixBy :: Name String -> p -> p
  suffixBy x p = rename (name p <> x) p
