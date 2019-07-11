-----------------------------------------------------------------------------
-- |
-- Module      :  VecaTests
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Test file for the Veca module.
-----------------------------------------------------------------------------
module VecaTests (vecaTests)
where

import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC

import           Data.Map                        (Map, fromList)
import           Data.Monoid                     ((<>))
import           Models.Events
import           Models.LabelledTransitionSystem as LTS (LabelledTransitionSystem (..),
                                                         State (..),
                                                         Transition (..))
import           Models.Name
import           Models.Named                    (Named (..), prefixBy)
import           Models.TimedAutomaton           as TA (Bounds (..), Edge (..),
                                                        Expression (..),
                                                        Location (..),
                                                        TimedAutomaton (..),
                                                        VariableAssignment (..),
                                                        VariableType (..),
                                                        VariableTyping (..),
                                                        relabel)
import           Trees.Tree
import           Veca.Model
import           Veca.Operations

vecaTests :: TestTree
vecaTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests for the Veca module" [uCToTA, uCToCTree, uCTreeToTAList]

uCToTA :: TestTree
uCToTA =
  testGroup "Unit tests for cToTA"
            [testCase "case 2.1" $ cToTA c1 @?= ta1
            ,testCase "case 2.1 with internal event" $ cToTA c1tau @?= ta1tau
            ,testCase "case 2.2" $ cToTA c2 @?= ta2
            ,testCase "case 2.3" $ cToTA c3 @?= ta3
            ,testCase "case 2.4" $ cToTA c4 @?= ta4]

uCToCTree :: TestTree
uCToCTree = testGroup
  "Unit tests for cToCTree"
  [testCase "case 2.7 (common names in subtrees)" $ cToCTree c7 @?= tree1]

uCTreeToTAList :: TestTree
uCTreeToTAList = testGroup
  "Unit tests for cTreeToTAList"
  [ testCase "case 2.7 (common names in subtrees)"
    $   cTreeToTAList tree1
    @?= tas1
  ]

b1 :: VName
b1 = Name ["1"]

b2 :: VName
b2 = Name ["2"]

b3 :: VName
b3 = Name ["3"]

b4 :: VName
b4 = Name ["4"]

a :: Operation
a = Operation $ Name ["a"]

b :: Operation
b = Operation $ Name ["b"]

c :: Operation
c = Operation $ Name ["c"]

m1 :: Message
m1 = Message (Name ["m1"]) (MessageType "...")

listDone :: Int -> Map (Name String) VariableTyping
listDone n = fromList
  [ ( Name ["done"]
    , VariableTyping (Name ["done"]) (IntType bounds) (Just $ Expression "0")
    )
  ]
  where bounds = Bounds 0 n

setDone :: Int -> VariableAssignment
setDone n = VariableAssignment (Name ["done"]) (Expression (show n))

ta1 :: VTA
ta1 = TimedAutomaton
  n1
  [Location "0", Location "1", Location "2"]
  (Location "0")
  []
  []
  []
  (listDone 3)
  [CTReceive a, CTReceive c, CTTau]
  [ Edge (Location "0") (CTReceive a) [] [] [setDone 1] (Location "1")
  , Edge (Location "1") (CTReceive c) [] [] [setDone 2] (Location "2")
  , Edge (Location "2") CTTau         [] [] [setDone 3] (Location "2")
  ]
  []

ta1tau :: VTA
ta1tau = TimedAutomaton
  n1
  [Location "0", Location "1", Location "2", Location "3"]
  (Location "0")
  []
  []
  []
  (listDone 3)
  [CTReceive a, CTReceive c, CTTau]
  [ Edge (Location "0") (CTReceive a) [] [] [setDone 1] (Location "1")
  , Edge (Location "1") (CTTau) [] [] [setDone 3] (Location "2")
  , Edge (Location "2") (CTReceive c) [] [] [setDone 2] (Location "3")
  , Edge (Location "3") CTTau         [] [] [setDone 3] (Location "3")
  ]
  []

ta2 :: VTA
ta2 = TimedAutomaton
  n2
  [Location "0", Location "1", Location "2"]
  (Location "0")
  []
  []
  []
  (listDone 3)
  [CTInvoke a, CTReceive b, CTTau]
  [ Edge (Location "0") (CTInvoke a)  [] [] [setDone 1] (Location "1")
  , Edge (Location "1") (CTReceive b) [] [] [setDone 2] (Location "2")
  , Edge (Location "2") CTTau         [] [] [setDone 3] (Location "2")
  ]
  []

ta3 :: VTA
ta3 = TimedAutomaton
  n3
  [Location "0", Location "1"]
  (Location "0")
  []
  []
  []
  (listDone 2)
  [CTReceive a, CTTau]
  [ Edge (Location "0") (CTReceive a) [] [] [setDone 1] (Location "1")
  , Edge (Location "1") CTTau         [] [] [setDone 2] (Location "1")
  ]
  []

ta4 :: VTA
ta4 = TimedAutomaton
  n4
  [Location "0", Location "1", Location "2"]
  (Location "0")
  []
  []
  []
  (listDone 3)
  [CTInvoke a, CTInvoke b, CTTau]
  [ Edge (Location "0") (CTInvoke a) [] [] [setDone 1] (Location "1")
  , Edge (Location "1") (CTInvoke b) [] [] [setDone 2] (Location "2")
  , Edge (Location "2") CTTau        [] [] [setDone 3] (Location "2")
  ]
  []

n1 :: VName
n1 = Name ["c1"]
nameC1 :: VName
nameC1 = Name ["Type1"]

c1 :: ComponentInstance
c1 = ComponentInstance n1 $ BasicComponent nameC1 sig beh
 where
  sig = Signature [a, c]
                  []
                  (fromList [(a, m1), (c, m1)])
                  (fromList [(a, Nothing), (c, Nothing)])
  beh = LabelledTransitionSystem
    n1
    [EventLabel . CReceive $ a, EventLabel . CReceive $ c]
    [State "0", State "1", State "2"]
    (State "0")
    [State "2"]
    [ Transition (State "0") (EventLabel . CReceive $ a) (State "1")
    , Transition (State "1") (EventLabel . CReceive $ c) (State "2")
    ]

c1tau :: ComponentInstance
c1tau = ComponentInstance n1 $ BasicComponent nameC1 sig beh
 where
  sig = Signature [a, c]
                  []
                  (fromList [(a, m1), (c, m1)])
                  (fromList [(a, Nothing), (c, Nothing)])
  beh = LabelledTransitionSystem
    n1
    [EventLabel . CReceive $ a, EventLabel . CReceive $ c, InternalLabel 2 4]
    [State "0", State "1", State "2", State "3"]
    (State "0")
    [State "3"]
    [ Transition (State "0") (EventLabel . CReceive $ a) (State "1")
    , Transition (State "1") (InternalLabel 2 4) (State "2")
    , Transition (State "2") (EventLabel . CReceive $ c) (State "3")
    ]

n2 :: VName
n2 = Name ["c2"]
nameC2 :: VName
nameC2 = Name ["Type2"]

c2 :: ComponentInstance
c2 = ComponentInstance n2 $ BasicComponent nameC2 sig beh
 where
  sig = Signature [b]
                  [a]
                  (fromList [(a, m1), (b, m1)])
                  (fromList [(a, Nothing), (b, Nothing)])
  beh = LabelledTransitionSystem
    n2
    [EventLabel . CInvoke $ a, EventLabel . CReceive $ b]
    [State "0", State "1", State "2"]
    (State "0")
    [State "2"]
    [ Transition (State "0") (EventLabel . CInvoke $ a)  (State "1")
    , Transition (State "1") (EventLabel . CReceive $ b) (State "2")
    ]

n3 :: VName
n3 = Name ["c3"]
nameC3 :: VName
nameC3 = Name ["Type3"]

c3 :: ComponentInstance
c3 = ComponentInstance n3 $ BasicComponent nameC3 sig beh
 where
  sig = Signature [a] [] (fromList [(a, m1)]) (fromList [(a, Nothing)])
  beh = LabelledTransitionSystem
    n3
    [EventLabel . CReceive $ a]
    [State "0", State "1"]
    (State "0")
    [State "1"]
    [Transition (State "0") (EventLabel . CReceive $ a) (State "1")]

n4 :: VName
n4 = Name ["c4"]
nameC4 :: VName
nameC4 = Name ["Type4"]

c4 :: ComponentInstance
c4 = ComponentInstance n4 $ BasicComponent nameC4 sig beh
 where
  sig = Signature []
                  [a, b]
                  (fromList [(a, m1), (b, m1)])
                  (fromList [(a, Nothing), (b, Nothing)])
  beh = LabelledTransitionSystem
    n4
    [EventLabel . CInvoke $ a, EventLabel . CInvoke $ b]
    [State "0", State "1", State "2"]
    (State "0")
    [State "2"]
    [ Transition (State "0") (EventLabel . CInvoke $ a) (State "1")
    , Transition (State "1") (EventLabel . CInvoke $ b) (State "2")
    ]

n5 :: VName
n5 = Name ["c5"]

c5 :: ComponentInstance
c5 = ComponentInstance n5 $ CompositeComponent n5 sig cs inb exb
 where
  sig = Signature [b, c]
                  []
                  (fromList [(b, m1), (c, m1)])
                  (fromList [(b, Nothing), (c, Nothing)])
  cs  = [c1, c2]
  inb = [Binding Internal b1 (JoinPoint n2 a) (JoinPoint n1 a)]
  exb =
    [ Binding External b2 (JoinPoint self b) (JoinPoint n2 b)
    , Binding External b3 (JoinPoint self c) (JoinPoint n1 c)
    ]

n6 :: VName
n6 = Name ["c6"]

c6 :: ComponentInstance
c6 = ComponentInstance n6 $ CompositeComponent n6 sig cs inb exb
 where
  sig = Signature [] [b] (fromList [(b, m1)]) (fromList [(b, Nothing)])
  cs  = [c3, c4]
  inb = [Binding Internal b1 (JoinPoint n4 a) (JoinPoint n3 a)]
  exb = [Binding External b2 (JoinPoint n4 b) (JoinPoint self b)]

n7 :: VName
n7 = Name ["c7"]

c7 :: ComponentInstance
c7 = ComponentInstance n7 $ CompositeComponent n7 sig cs inb exb
 where
  sig = Signature [c] [] (fromList [(c, m1)]) (fromList [(c, Nothing)])
  cs  = [c5, c6]
  inb = [Binding Internal b1 (JoinPoint n6 b) (JoinPoint n5 b)]
  exb = [Binding External b2 (JoinPoint self c) (JoinPoint n5 c)]

tree1 :: VCTree
tree1 = Node c7 [(n5, st5), (n6, st6)]
 where
  st5 = Node c5 [(n1, st1), (n2, st2)]
  st6 = Node c6 [(n3, st3), (n4, st4)]
  st1 = Leaf c1
  st2 = Leaf c2
  st3 = Leaf c3
  st4 = Leaf c4

tree1' :: VTATree
tree1' = Node c7 [(n5, st5'), (n6, st6')]
 where
  st5' = Node c5 [(n1, st1'), (n2, st2')]
  st6' = Node c6 [(n3, st3'), (n4, st4')]
  st1' = Leaf ta1
  st2' = Leaf ta2
  st3' = Leaf ta3
  st4' = Leaf ta4

ta1' :: VTA
ta1' = prefixBy (n7 <> n5) $ relabel sub1 ta1
 where
  sub1 =
    [ (CTReceive a, CTReceive $ indexBy (n7 <> n5 <> b1) a)
    , (CTReceive c, CTReceive $ indexBy (n7 <> b2) c)
    ]

ta2' :: VTA
ta2' = prefixBy (n7 <> n5) $ relabel sub2 ta2
 where
  sub2 =
    [ (CTInvoke a , CTInvoke $ indexBy (n7 <> n5 <> b1) a)
    , (CTReceive b, CTReceive $ indexBy (n7 <> b1) b)
    ]

ta3' :: VTA
ta3' = prefixBy (n7 <> n6) $ relabel sub3 ta3
  where sub3 = [(CTReceive a, CTReceive $ indexBy (n7 <> n6 <> b1) a)]

ta4' :: VTA
ta4' = prefixBy (n7 <> n6) $ relabel sub4 ta4
 where
  sub4 =
    [ (CTInvoke a, CTInvoke $ indexBy (n7 <> n6 <> b1) a)
    , (CTInvoke b, CTInvoke $ indexBy (n7 <> b1) b)
    ]

tas1 :: [VTA]
tas1 = [ta1', ta2', ta3', ta4']
