{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Models.Name
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Types for names.
-----------------------------------------------------------------------------

module Models.Name (
    -- * constructors
    Name(..)
    -- * validity checking
  , isValidName)
where

import           Data.Hashable               (Hashable)
import           Data.Monoid                 (All (..), getAll, (<>))
import           GHC.Generics                (Generic)
import           Transformations.ModelToText (foldMapToString')

-- |A name is a list of strings.
-- A specific name is the name with an empty list of string.
-- It can be used to ensure the existance of a name different from "regular" names.
newtype Name = Name [String]
  deriving (Eq,Ord,Generic)

-- |Show instance for names.
instance Show Name where
  show (Name []) = "_"
  show (Name ns) = foldMapToString' "." id ns

-- |Monoid instance for names.
instance Monoid Name where
  mempty = Name []
  mappend (Name ns1) (Name ns2) = Name $ ns1 <> ns2

-- |Hash for names.
instance Hashable Name

-- |Check the validity of a name.
isValidName :: Name -> Bool
isValidName (Name ns) = getAll $ foldMap (All . not . null) ns
