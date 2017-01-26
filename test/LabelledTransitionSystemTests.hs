-----------------------------------------------------------------------------
-- |
-- Module      :  LabelledTransitionSystemTests
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Test file for the LabelledTransitionSystem module.
-----------------------------------------------------------------------------

module LabelledTransitionSystemTests (labelledTransitionSystemTests)
where

import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC

import           Data.Set                 as S (Set, empty, fromList)
import           LabelledTransitionSystem as L
import           Numeric.Natural

labelledTransitionSystemTests :: TestTree
labelledTransitionSystemTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests"
            [uSuccessorStates
            ,uReachableStates]

--
as :: Set String
as = S.fromList ["a","b","c","d"]

s1 :: State Natural
s1 = State 1

s2 :: State Natural
s2 = State 2

s3 :: State Natural
s3 = State 3

s4 :: State Natural
s4 = State 4

s5 :: State Natural
s5 = State 5

s6 :: State Natural
s6 = State 6

ss :: Set (State Natural)
ss = S.fromList [s1,s2,s3,s4,s5,s6]

fs :: Set (State Natural)
fs = S.fromList [s1]

ts :: Set (Transition String Natural)
ts =
  S.fromList
    [Transition s2 "a" s1
    ,Transition s2 "b" s3
    ,Transition s3 "a" s2
    ,Transition s3 "b" s2
    ,Transition s3 "c" s4
    ,Transition s3 "d" s5
    ,Transition s4 "a" s2
    ,Transition s4 "a" s5
    ,Transition s4 "b" s6
    ,Transition s5 "a" s3
    ,Transition s5 "a" s3
    ,Transition s6 "a" s6]

lts :: LabelledTransitionSystem String Natural
lts = LabelledTransitionSystem as ss s2 fs ts

--
uSuccessorStates :: TestTree
uSuccessorStates =
  testGroup "Unit tests for successorStates"
            [(testCase "no outgoing transitions" $
              (successorStates ts s1) @?= S.empty)
            ,(testCase "several outgoing transitions (regular)" $
              (successorStates ts s2) @?= (S.fromList [s3,s1]))
            ,(testCase "several outgoing transitions (duplicates, deterministic)" $
              (successorStates ts s3) @?= (S.fromList [s2,s4,s5]))
            ,(testCase "several outgoing transitions (non deterministic, different targets)" $
              (successorStates ts s4) @?= (S.fromList [s5,s2,s6]))
            ,(testCase "several outgoing transitions (non deterministic, same target)" $
              (successorStates ts s5) @?= (S.fromList [s3]))
            ,(testCase "loop" $
              (successorStates ts s6) @?= (S.fromList [s6]))]

--
uReachableStates :: TestTree
uReachableStates = testGroup "Unit tests for reachableStates"
            [(testCase "no outgoing transitions" $
              (reachableStates ts s1) @?= S.empty)
            ,(testCase "several outgoing transitions (regular)" $
              (reachableStates ts s2) @?= (S.fromList [s1,s2,s3,s4,s5,s6]))
            ,(testCase "several outgoing transitions (duplicates, deterministic)" $
              (reachableStates ts s3) @?= (S.fromList [s1,s2,s3,s4,s5,s6]))
            ,(testCase "several outgoing transitions (non deterministic, different targets)" $
              (reachableStates ts s4) @?= (S.fromList [s1,s2,s3,s4,s5,s6]))
            ,(testCase "several outgoing transitions (non deterministic, same target)" $
              (reachableStates ts s5) @?= (S.fromList [s1,s2,s3,s4,s5,s6]))
            ,(testCase "loop" $
              (reachableStates ts s6) @?= (S.fromList [s6]))]
