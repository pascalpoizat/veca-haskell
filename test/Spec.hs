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

import           LabelledTransitionSystemTests
--import           RoverTests
import           Test.Tasty
import           TimedAutomatonTests
import           TreeTests

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Tests" [treeTests
                         ,labelledTransitionSystemTests
                         ,timedAutomatonTests
                         --,roverTests
                         ]

