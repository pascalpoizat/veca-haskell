{-|
Module      : Models.Communication
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
import           Models.Internal

{-|
A class for communication (input-output) events.

Minimal definition: 'isInput' or 'isOutput'.
Note that 'isInternal' and 'complementary' must also be defined
due to the relation to 'Complementary' and 'Internal'.

While defining your instances (for 'Complementary', 'Internal', and 'Communication') you should ensure:

@
forall x, 'isInput' x implies not ('isOutput' x) and not ('isInternal' x)

forall x, 'isOutput' x implies not ('isInput' x) and not ('isInternal' x)

forall x, 'isInternal' x implies not ('isOutput' x) and not ('isInput' x)

forall x, either 'isInternal' x, 'isInput' x, or 'isOutput' x is true

forall x, 'isInput' x implies 'isOutput' $ 'complementary' x

forall x, 'isOutput' x implies 'isInput' $ 'complementary' x

forall x, 'isInternal' x implies 'isInternal' $ 'complementary' x
@
-}
class (Complementary p, Internal p) =>
      Communication p where
  {-# MINIMAL isInput | isOutput #-}
  isInput :: p -> Bool
  isOutput :: p -> Bool
  isInput p = not (isOutput p || isInternal p)
  isOutput p = not (isInput p || isInternal p)
