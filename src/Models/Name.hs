{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Models.Name
Description : A type for (indexed) names.
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Models.Name (
    -- * constructors
    Name(..)
    -- * validity checking
  , isValidName)
where

import           Data.Aeson
import           Data.Hashable               (Hashable)
import           Data.Monoid                 (All (..), getAll, (<>))
import           GHC.Generics                (Generic)
import           Transformations.ModelToText (foldMapToString')

{-|
A name is a list.

A specific name is the name with an empty list.
It can be used to ensure the existance of a name different from "regular" names.
-}
newtype Name a =
  Name [a]
  deriving (Eq, Ord, Generic)

{-|
Show instance for names.
-}
instance Show a => Show (Name a) where
  show (Name []) = "_"
  show (Name ns) = foldMapToString' "." show ns

{-|
FromJSON instance for names.
-}
instance FromJSON a => FromJSON (Name a)

{-|
ToJSON instance for names.
-}
instance ToJSON a => ToJSON (Name a)

{-|
Semigroup instance for names.
-}
instance Semigroup (Name a) where
  (Name ns1) <> (Name ns2) = Name $ ns1 <> ns2

{-|
Monoid instance for names.
-}
instance Monoid (Name a) where
  mempty = Name []

{-|
Hash instance for names.
-}
instance Hashable a => Hashable (Name a)

{-|
Check the validity of a name.
-}
isValidName :: Name String -> Bool
isValidName (Name ns) = getAll $ foldMap (All . not . null) ns
