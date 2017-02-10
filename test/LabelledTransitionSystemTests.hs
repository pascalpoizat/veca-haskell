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
            [uSuccessors
            ,uPredecessors
            ,uReachables
            ,uCoreachables
            ,uIsSelfReachable
            ,uHasLoop,
            uPaths]

--
as :: Set String
as = S.fromList ["a","b","c","d","z"]

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
    ,Transition s4 "z" s2
    ,Transition s4 "z" s5
    ,Transition s4 "b" s6
    ,Transition s5 "a" s3
    ,Transition s5 "a" s3
    ,Transition s6 "a" s6
    ,Transition s6 "a" s5]

lts1 :: LabelledTransitionSystem String Natural
lts1 = LabelledTransitionSystem as ss s2 fs ts

ts2 :: Set (Transition String Natural)
ts2 = S.fromList [Transition s1 "a" s2,Transition s2 "b" s3]

lts2 :: LabelledTransitionSystem String Natural
lts2 =
  LabelledTransitionSystem as
                           (S.fromList [s1,s2,s3])
                           s1
                           S.empty
                           ts2

ts3 :: Set (Transition String Natural)
ts3 =
  S.fromList [Transition s1 "a" s2,Transition s2 "b" s3,Transition s3 "a" s3]

lts3 :: LabelledTransitionSystem String Natural
lts3 =
  LabelledTransitionSystem as
                           (S.fromList [s1,s2,s3])
                           s1
                           S.empty
                           ts3

ts4 :: Set (Transition String Natural)
ts4 =
  S.fromList [Transition s1 "a" s2,Transition s2 "b" s3,Transition s3 "a" s1]

lts4 :: LabelledTransitionSystem String Natural
lts4 =
  LabelledTransitionSystem as
                           (S.fromList [s1,s2,s3])
                           s1
                           S.empty
                           ts4

--
uSuccessors :: TestTree
uSuccessors =
  testGroup "Unit tests for successors"
            [(testCase "no outgoing transitions" $
              (successors ts s1) @?= S.empty)
            ,(testCase "several outgoing transitions (regular)" $
              (successors ts s2) @?= (S.fromList [s3,s1]))
            ,(testCase "several outgoing transitions (duplicates, deterministic)" $
              (successors ts s3) @?= (S.fromList [s2,s4,s5]))
            ,(testCase "several outgoing transitions (non deterministic, different targets)" $
              (successors ts s4) @?= (S.fromList [s5,s2,s6]))
            ,(testCase "several outgoing transitions (non deterministic, same target)" $
              (successors ts s5) @?= (S.fromList [s3]))
            ,(testCase "loop" $
              (successors ts s6) @?= (S.fromList [s6,s5]))]

uPredecessors :: TestTree
uPredecessors =
  testGroup "Unit tests for predecessors"
            [(testCase "one incoming transition" $
              (predecessors ts s1) @?= S.fromList [s2])
            ,(testCase "several incoming transitions (duplicates, deterministic)" $
              (predecessors ts s2) @?= (S.fromList [s3,s4]))
            ,(testCase "several incoming transitions (non deterministic, same source)" $
              (predecessors ts s3) @?= (S.fromList [s2,s5]))
            ,(testCase "several outgoing transitions (regular)" $
              (predecessors ts s4) @?= (S.fromList [s3]))
            ,(testCase "several incoming transitions (non deterministic, different sources)" $
              (predecessors ts s5) @?= (S.fromList [s3,s4,s6]))
            ,(testCase "loop" $
              (predecessors ts s6) @?= (S.fromList [s4,s6]))]

uReachables :: TestTree
uReachables = testGroup "Unit tests for reachables"
            [(testCase "no outgoing transitions" $
              (reachables ts s1) @?= S.empty)
            ,(testCase "several outgoing transitions (regular)" $
              (reachables ts s2) @?= (S.fromList [s1,s2,s3,s4,s5,s6]))
            ,(testCase "several outgoing transitions (duplicates, deterministic)" $
              (reachables ts s3) @?= (S.fromList [s1,s2,s3,s4,s5,s6]))
            ,(testCase "several outgoing transitions (non deterministic, different targets)" $
              (reachables ts s4) @?= (S.fromList [s1,s2,s3,s4,s5,s6]))
            ,(testCase "several outgoing transitions (non deterministic, same target)" $
              (reachables ts s5) @?= (S.fromList [s1,s2,s3,s4,s5,s6]))
            ,(testCase "loop" $
              (reachables ts s6) @?= (S.fromList [s1,s2,s3,s4,s5,s6]))]

uCoreachables :: TestTree
uCoreachables =
  testGroup "Unit tests for coreachables"
            [(testCase "one incoming transition" $
              (coreachables ts s1) @?= S.fromList [s2,s3,s4,s5,s6])
            ,(testCase "several incoming transitions (duplicates, deterministic)" $
              (coreachables ts s2) @?= (S.fromList [s2,s3,s4,s5,s6]))
            ,(testCase "several incoming transitions (non deterministic, same source)" $
              (coreachables ts s3) @?= (S.fromList [s2,s3,s4,s5,s6]))
            ,(testCase "several outgoing transitions (regular)" $
              (coreachables ts s4) @?= (S.fromList [s2,s3,s4,s5,s6]))
            ,(testCase "several incoming transitions (non deterministic, different sources)" $
              (coreachables ts s5) @?= (S.fromList [s2,s3,s4,s5,s6]))
            ,(testCase "loop" $
              (coreachables ts s6) @?= (S.fromList [s2,s3,s4,s5,s6]))]

uIsSelfReachable :: TestTree
uIsSelfReachable =
  testGroup "Unit tests for isSelfReachable"
            [(testCase "no loop"      $ isSelfReachable ts2 s1 @?= False)
            ,(testCase "no loop"      $ isSelfReachable ts2 s2 @?= False)
            ,(testCase "no loop"      $ isSelfReachable ts2 s3 @?= False)
            ,(testCase "self loop"    $ isSelfReachable ts3 s1 @?= False)
            ,(testCase "self loop"    $ isSelfReachable ts3 s2 @?= False)
            ,(testCase "self loop"    $ isSelfReachable ts3 s3 @?= True)
            ,(testCase "regular loop" $ isSelfReachable ts4 s1 @?= True)
            ,(testCase "regular loop" $ isSelfReachable ts4 s2 @?= True)
            ,(testCase "regular loop" $ isSelfReachable ts4 s3 @?= True)]

uHasLoop :: TestTree
uHasLoop =
  testGroup "Unit tests for hasLoop"
            [(testCase "no loop"      $ hasLoop lts2 @?= False)
            ,(testCase "self loop"    $ hasLoop lts3 @?= True)
            ,(testCase "regular loop" $ hasLoop lts4 @?= True)]

uPaths :: TestTree
uPaths =
  testGroup "Unit tests for paths"
            [(testCase "no loop" $
              paths lts2 @?=
              fromList [Path []
                       ,Path [(State 1,"a",State 2)]
                       ,Path [(State 1,"a",State 2),(State 2,"b",State 3)]])]
