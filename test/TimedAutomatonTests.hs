-----------------------------------------------------------------------------
-- |
-- Module      :  TimedAutomatonTests
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Test file for the TimedAutomaton module.
-----------------------------------------------------------------------------

module TimedAutomatonTests (timedAutomatonTests)
where

import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC

import           Data.Map                        (empty)
import           Models.LabelledTransitionSystem
import           Models.TimedAutomaton
import           Veca.Veca

timedAutomatonTests :: TestTree
timedAutomatonTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests"
            [uAsXta]

uAsXta :: TestTree -- TODO
uAsXta =
  testGroup "Unit tests for toXta"
            [(testCase "TA with a single internal transition" $
              (asXta ta_model001) @?= res1)
--            ,(testCase "... test case description ..." $
--              (asXta ta_model002) @?= res2)
            ]
  where
        --
        l0 = Location "0"
        l1 = Location "1"
        l2 = Location "2"
        c1 = Clock "c1"
        tau = CTau :: BehaviorEvent
        --
        ta_model001 =
          TimedAutomaton "Model001"
                         [l0,l1]
                         l0
                         []
                         [tau]
                         [Edge l0 tau [] [] l1]
                         empty
        res1 =
          unlines ["chan tau;"
                  ,"process Model001(){"
                  ,"state l_0, l_1;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { sync tau; };"
                  ,"}"
                  ,"Process = Model001();"
                  ,"system Process;"]
        --
        ta_model002 =
          TimedAutomaton
            "Model002"
            [l0,l1,l2]
            l0
            [c1]
            [tau]
               [Edge l0 tau [] [c1] l1
               ,Edge l1 tau [ClockConstraint c1 LE 5] [] l2]
            empty
        res2 = unlines [] -- TODO
