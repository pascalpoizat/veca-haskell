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

import           Data.Set                        (fromList)
import           Models.LabelledTransitionSystem
import           Numeric.Natural
import Models.Name

labelledTransitionSystemTests :: TestTree
labelledTransitionSystemTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests = testGroup
  "Unit tests"
  [ uSuccessors
  , uPredecessors
  , uReachables
  , uCoreachables
  , uIsSelfReachable
  , uHasLoop
  , uPaths
  , uPathStates
  , uPathStatesUnique
  , uTrace
  ]

--
as :: [String]
as = ["a", "b", "c", "d", "z"]

n1 :: Name String
n1 = Name ["1"]

s0 :: State Natural
s0 = State 0

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

ss :: [State Natural]
ss = [s1, s2, s3, s4, s5, s6]

fs :: [State Natural]
fs = [s1]

ts :: [Transition String Natural]
ts =
  [ Transition s2 "a" s1
  , Transition s2 "b" s3
  , Transition s3 "a" s2
  , Transition s3 "b" s2
  , Transition s3 "c" s4
  , Transition s3 "d" s5
  , Transition s4 "z" s2
  , Transition s4 "z" s5
  , Transition s4 "b" s6
  , Transition s5 "a" s3
  , Transition s5 "a" s3
  , Transition s6 "a" s6
  , Transition s6 "a" s5
  ]

lts1 :: LabelledTransitionSystem String Natural
lts1 = LabelledTransitionSystem n1 as ss s2 fs ts

ts2 :: [Transition String Natural]
ts2 = [Transition s1 "a" s2, Transition s2 "b" s3]

lts2 :: LabelledTransitionSystem String Natural
lts2 = LabelledTransitionSystem n1 as [s1, s2, s3] s1 [] ts2

ts3 :: [Transition String Natural]
ts3 = [Transition s1 "a" s2, Transition s2 "b" s3, Transition s3 "a" s3]

lts3 :: LabelledTransitionSystem String Natural
lts3 = LabelledTransitionSystem n1 as [s1, s2, s3] s1 [] ts3

ts4 :: [Transition String Natural]
ts4 = [Transition s1 "a" s2, Transition s2 "b" s3, Transition s3 "a" s1]

lts4 :: LabelledTransitionSystem String Natural
lts4 = LabelledTransitionSystem n1 as [s1, s2, s3] s1 [] ts4

p1 :: Path String Natural
p1 = Path []

p2 :: Path String Natural
p2 = Path [Transition s0 "a" s1, Transition s1 "b" s2]

p3 :: Path String Natural
p3 = p2 <> Path [Transition s2 "c" s3]

p4 :: Path String Natural
p4 = p2 <> Path [Transition s2 "c" s0, Transition s0 "d" s3]

p5 :: Path String Natural
p5 = Path [Transition s2 "c" s0]

p6 :: Path String Natural
p6 = p2 <> p5 <> p2

p7 :: Path String Natural
p7 = Path [Transition s0 "a" s0]

p8 :: Path String Natural
p8 = p7 <> p7 <> p7

emptyStateList :: [State Natural]
emptyStateList = []

emptyStringList :: [String]
emptyStringList = []
--
uSuccessors :: TestTree
uSuccessors = testGroup
  "Unit tests for successors"
  [ testCase "no outgoing transitions" $ successors ts s1 @?= []
  , testCase "several outgoing transitions (regular)"
  $   fromList (successors ts s2)
  @?= fromList [s3, s1]
  , testCase "several outgoing transitions (duplicates, deterministic)"
  $   fromList (successors ts s3)
  @?= fromList [s2, s4, s5]
  , testCase
    "several outgoing transitions (non deterministic, different targets)"
  $   fromList (successors ts s4)
  @?= fromList [s5, s2, s6]
  , testCase "several outgoing transitions (non deterministic, same target)"
  $   fromList (successors ts s5)
  @?= fromList [s3]
  , testCase "loop" $ fromList (successors ts s6) @?= fromList [s6, s5]
  ]

uPredecessors :: TestTree
uPredecessors = testGroup
  "Unit tests for predecessors"
  [ testCase "one incoming transition"
  $   fromList (predecessors ts s1)
  @?= fromList [s2]
  , testCase "several incoming transitions (duplicates, deterministic)"
  $   fromList (predecessors ts s2)
  @?= fromList [s3, s4]
  , testCase "several incoming transitions (non deterministic, same source)"
  $   fromList (predecessors ts s3)
  @?= fromList [s2, s5]
  , testCase "several outgoing transitions (regular)"
  $   fromList (predecessors ts s4)
  @?= fromList [s3]
  , testCase
    "several incoming transitions (non deterministic, different sources)"
  $   fromList (predecessors ts s5)
  @?= fromList [s3, s4, s6]
  , testCase "loop" $ fromList (predecessors ts s6) @?= fromList [s4, s6]
  ]

uReachables :: TestTree
uReachables = testGroup
  "Unit tests for reachables"
  [ testCase "no outgoing transitions" $ reachables ts s1 @?= []
  , testCase "several outgoing transitions (regular)"
  $   fromList (reachables ts s2)
  @?= fromList [s1, s2, s3, s4, s5, s6]
  , testCase "several outgoing transitions (duplicates, deterministic)"
  $   fromList (reachables ts s3)
  @?= fromList [s1, s2, s3, s4, s5, s6]
  , testCase
    "several outgoing transitions (non deterministic, different targets)"
  $   fromList (reachables ts s4)
  @?= fromList [s1, s2, s3, s4, s5, s6]
  , testCase "several outgoing transitions (non deterministic, same target)"
  $   fromList (reachables ts s5)
  @?= fromList [s1, s2, s3, s4, s5, s6]
  , testCase "loop" $ fromList (reachables ts s6) @?= fromList
    [s1, s2, s3, s4, s5, s6]
  ]

uCoreachables :: TestTree
uCoreachables = testGroup
  "Unit tests for coreachables"
  [ testCase "one incoming transition"
  $   fromList (coreachables ts s1)
  @?= fromList [s2, s3, s4, s5, s6]
  , testCase "several incoming transitions (duplicates, deterministic)"
  $   fromList (coreachables ts s2)
  @?= fromList [s2, s3, s4, s5, s6]
  , testCase "several incoming transitions (non deterministic, same source)"
  $   fromList (coreachables ts s3)
  @?= fromList [s2, s3, s4, s5, s6]
  , testCase "several outgoing transitions (regular)"
  $   fromList (coreachables ts s4)
  @?= fromList [s2, s3, s4, s5, s6]
  , testCase
    "several incoming transitions (non deterministic, different sources)"
  $   fromList (coreachables ts s5)
  @?= fromList [s2, s3, s4, s5, s6]
  , testCase "loop" $ fromList (coreachables ts s6) @?= fromList
    [s2, s3, s4, s5, s6]
  ]

uIsSelfReachable :: TestTree
uIsSelfReachable = testGroup
  "Unit tests for isSelfReachable"
  [ testCase "no loop" $ isSelfReachable ts2 s1 @?= False
  , testCase "no loop" $ isSelfReachable ts2 s2 @?= False
  , testCase "no loop" $ isSelfReachable ts2 s3 @?= False
  , testCase "self loop" $ isSelfReachable ts3 s1 @?= False
  , testCase "self loop" $ isSelfReachable ts3 s2 @?= False
  , testCase "self loop" $ isSelfReachable ts3 s3 @?= True
  , testCase "regular loop" $ isSelfReachable ts4 s1 @?= True
  , testCase "regular loop" $ isSelfReachable ts4 s2 @?= True
  , testCase "regular loop" $ isSelfReachable ts4 s3 @?= True
  ]

uHasLoop :: TestTree
uHasLoop = testGroup
  "Unit tests for hasLoop"
  [ testCase "no loop" $ hasLoop lts2 @?= False
  , testCase "self loop" $ hasLoop lts3 @?= True
  , testCase "regular loop" $ hasLoop lts4 @?= True
  ]

uPaths :: TestTree
uPaths = testGroup
  "Unit tests for paths"
  [ testCase "no loop" $ fromList (paths lts2) @?= fromList
      [ Path []
      , Path [Transition s1 "a" s2]
      , Path [Transition s1 "a" s2, Transition s2 "b" s3]
      ]
  ]

uPathStates :: TestTree
uPathStates = testGroup
  "Unit tests for pathStates"
  [ testCase "empty path" $ pathStates p1 @?= emptyStateList
  , testCase "simple path" $ pathStates p3 @?= [s0, s1, s2, s3]
  , testCase "path with a duplicated state"
  $   pathStates p4
  @?= [s0, s1, s2, s0, s3]
  , testCase "path with a loop" $ pathStates p6 @?= [s0, s1, s2, s0, s1, s2]
  , testCase "path with a self loop" $ pathStates p7 @?= [s0, s0]
  , testCase "path with a repeated self loop"
  $   pathStates p8
  @?= [s0, s0, s0, s0]
  ]

uPathStatesUnique :: TestTree
uPathStatesUnique = testGroup
  "Unit tests for pathStatesUnique"
  [ testCase "empty path" $ pathStatesUnique p1 @?= emptyStateList
  , testCase "simple path" $ (fromList . pathStatesUnique $ p3) @?= fromList
    [s0, s1, s2, s3]
  , testCase "path with a duplicated state"
  $   (fromList . pathStatesUnique $ p4)
  @?= fromList [s0, s1, s2, s3]
  , testCase "path with a loop"
  $   (fromList . pathStatesUnique $ p6)
  @?= fromList [s0, s1, s2]
  , testCase "path with a self loop"
  $   (fromList . pathStatesUnique $ p7)
  @?= fromList [s0]
  , testCase "path with a repeated self loop"
  $   (fromList . pathStatesUnique $ p8)
  @?= fromList [s0]
  ]

uTrace :: TestTree
uTrace = testGroup
  "Unit tests for trace"
  [ testCase "empty path" $ trace p1 @?= emptyStringList
  , testCase "simple path" $ trace p3 @?= ["a", "b", "c"]
  , testCase "path with a duplicated state" $ trace p4 @?= ["a", "b", "c", "d"]
  , testCase "path with a loop" $ trace p6 @?= ["a", "b", "c", "a", "b"]
  , testCase "path with a self loop" $ trace p7 @?= ["a"]
  , testCase "path with a repeated self loop" $ trace p8 @?= ["a", "a", "a"]
  ]
