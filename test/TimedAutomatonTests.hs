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

import           Data.Map                 as M (empty)
import           Data.Set                 as S (empty, fromList)
import           LabelledTransitionSystem as L
import           TimedAutomaton
import           Veca

timedAutomatonTests :: TestTree
timedAutomatonTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests"
            [uToXta]

uToXta :: TestTree -- TODO
uToXta =
  testGroup "Unit tests for toXta"
            [(testCase "... test case description 1 ..." $ (toXta ta1) @?= res1)
            ,(testCase "... test case description 2 ..." $ (toXta ta2) @?= res2)]
  where
        --
        l0 = Location 0
        l1 = Location 1
        l2 = Location 2
        clock1 = Clock "c1"
        tau = CTau::BehaviorEvent
        --
        ta1 =
          TimedAutomaton (S.fromList [l0,l1])
                         l0
                         S.empty
                         (S.fromList [tau])
                         (S.fromList [Edge l0 tau S.empty S.empty l1])
                         M.empty
        res1 = concat ["... the result in XTA ...","... ...","... ..."]
        --
        ta2 =
          TimedAutomaton
            (S.fromList [l0,l1,l2])
            l0
            (S.fromList [clock1])
            (S.fromList [tau])
            (S.fromList
               [Edge l0 tau S.empty (S.fromList [clock1]) l1
               ,Edge l1 tau (S.fromList [ClockConstraint clock1 LE 5]) S.empty l2])
            M.empty
        res2 = concat ["... the result in XTA ...","... ...","... ..."]
