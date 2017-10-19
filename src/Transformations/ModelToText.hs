-----------------------------------------------------------------------------
-- |
-- Module      :  Transformations.ModelToText
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Model to text transformations.
-----------------------------------------------------------------------------

module Transformations.ModelToText (foldMapToString
                                   ,foldMapToString')
where

import           Data.Foldable
import           Data.List     (intercalate)
import           Data.Monoid

-- |Folds a structure to a `String` given
-- a prefix, a suffix, a separator, and a map function.
--
-- @foldMapToString "{" "," "}" show [1,2,3] = "{1,2,3}"@
foldMapToString
  :: (Foldable t)
  => String -> String -> String -> (a -> String) -> t a -> String
foldMapToString p s q f xs =
  if null xs
     then ""
     else p <> foldMapToString' s f xs <> q

-- |Folds a structure to a `String` given
-- a separator and a map function.
--
-- @foldMapToString' "," show [1,2,3] = "1,2,3"@
foldMapToString'
  :: (Foldable t)
  => String -> (a -> String) -> t a -> String
foldMapToString' s f xs =
  if null xs
     then ""
     else intercalate s
                      (f <$> toList xs)
