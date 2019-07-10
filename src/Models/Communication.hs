{-|
Module      : Models.TCommunication
Description : A class for communication (input-output) events.
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Models.Communication (
  Communication(..)
  )
where

import           Models.Complementary

{-|
A class for communication (input-output) events.

Minimal definition: 'isInput' or 'isOutput'.
Note that 'complementary' must also be defined
due to the relation to 'Complementary'.

While defining your instances (for 'Complementary' and 'Communication') you should ensure:

@
forall x, 'isInput' x implies not ('isOutput' x)

forall x, 'isOutput' x implies not ('isInput' x)

forall x, either 'isInput' x or 'isOutput' x is true

forall x, 'isInput' x implies 'isOutput' $ 'complementary' x

forall x, 'isOutput' x implies 'isInput' $ 'complementary' x

@
-}
class (Complementary p) =>
      Communication p where
  {-# MINIMAL isInput | isOutput #-}
  isInput :: p -> Bool
  isOutput :: p -> Bool
  isInput p = not (isOutput p)
  isOutput p = not (isInput p)
