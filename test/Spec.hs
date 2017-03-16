-----------------------------------------------------------------------------
-- |
-- Module      :  Spec
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Main test file for the Veca library.
-----------------------------------------------------------------------------

import           EventsTests
import           LabelledTransitionSystemTests
--import           RoverTests
import           ModelToTextTests
import           Test.Tasty
import           TimedAutomatonTests
import           TreeTests

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Tests" [modelToTextTests
                         ,treeTests
                         ,labelledTransitionSystemTests
                         ,eventsTests
                         ,timedAutomatonTests
                         --,roverTests
                         ]

