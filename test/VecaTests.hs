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
  testGroup "Unit tests for the Veca module" [uIsValidComponent, uCToTA, uCToCTree, uCTreeToTAList]

uIsValidComponent :: TestTree
uIsValidComponent =
  testGroup "Unit tests for isValidComponent"
            [testCase "case 2.1" $ isValidComponent (componentType c1) @?= True
            ,testCase "case 2.1 with internal event" $ isValidComponent (componentType c1tau) @?= True
            ,testCase "case 2.1 with timeout event" $ isValidComponent (componentType c1theta) @?= True
            ,testCase "case 2.2" $ isValidComponent (componentType c2) @?= True
            ,testCase "case 2.3" $ isValidComponent (componentType c3) @?= True
            ,testCase "case 2.4" $ isValidComponent (componentType c4) @?= True
            ,testCase "case error internal (int/event)" $ isValidComponent (componentType cerr1) @?= False
            ,testCase "case error internal (int/inf int)" $ isValidComponent (componentType cerr1b) @?= False
            ,testCase "case error timeout (2 timeouts)" $ isValidComponent (componentType cerr2) @?= False
            ,testCase "case error timeout (other than reception)" $ isValidComponent (componentType cerr3) @?= False
            ,testCase "case error timestep" $ isValidComponent (componentType cerr4) @?= False
            ]

uCToTA :: TestTree
uCToTA =
  testGroup "Unit tests for cToTA"
            [testCase "case 2.1" $ cToTA c1 @?= ta1
            ,testCase "case 2.1 with internal event" $ cToTA c1tau @?= ta1tau
            ,testCase "case 2.1 with timeout event" $ cToTA c1theta @?= ta1theta
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
  , Edge (Location "1") CTTau [] [] [setDone 3] (Location "2")
  , Edge (Location "2") (CTReceive c) [] [] [setDone 2] (Location "3")
  , Edge (Location "3") CTTau         [] [] [setDone 3] (Location "3")
  ]
  []

ta1theta :: VTA
ta1theta = TimedAutomaton
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
    [EventLabel . CReceive $ a, EventLabel . CReceive $ c, InternalLabel (TimeValue 2) (TimeValue 4)]
    [State "0", State "1", State "2", State "3"]
    (State "0")
    [State "3"]
    [ Transition (State "0") (EventLabel . CReceive $ a) (State "1")
    , Transition (State "1") (InternalLabel (TimeValue 2) (TimeValue 4)) (State "2")
    , Transition (State "2") (EventLabel . CReceive $ c) (State "3")
    ]

c1theta :: ComponentInstance
c1theta = ComponentInstance n1 $ BasicComponent nameC1 sig beh
 where
  sig = Signature [a, c]
                  []
                  (fromList [(a, m1), (c, m1)])
                  (fromList [(a, Just m1), (c, Nothing)])
  beh = LabelledTransitionSystem
    n1
    [ EventLabel . CReceive $ a
    , EventLabel . CReply $ a
    , EventLabel . CReceive $ c
    , TimeoutLabel 15
    , InternalLabel (TimeValue 2) (TimeValue 4)
    , InternalLabel (TimeValue 0) InfiniteValue]
    [State "0", State "1", State "2", State "3", State "4", State "5"]
    (State "0")
    [State "5"]
    [ Transition (State "0") (EventLabel . CReceive $ a) (State "1")
    , Transition (State "1") (EventLabel . CReceive $ c) (State "2")
    , Transition (State "1") (TimeoutLabel 15) (State "3")
    , Transition (State "2") (InternalLabel (TimeValue 0) InfiniteValue) (State "4")
    , Transition (State "3") (InternalLabel (TimeValue 2) (TimeValue 4)) (State "4")
    , Transition (State "4") (EventLabel . CReply $ a) (State "5")
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

-- error internal (int/event)
cerr1 :: ComponentInstance
cerr1 = ComponentInstance n1 $ BasicComponent nameC1 sig beh
 where
  sig = Signature [a]
                  []
                  (fromList [(a, m1)])
                  (fromList [(a, Nothing)])
  beh = LabelledTransitionSystem
    n1
    [EventLabel . CReceive $ a, InternalLabel (TimeValue 2) (TimeValue 4)]
    [State "0", State "1", State "2"]
    (State "0")
    [State "1", State "2"]
    [ Transition (State "0") (InternalLabel (TimeValue 2) (TimeValue 4)) (State "1")
    , Transition (State "0") (EventLabel . CReceive $ c) (State "2")
    ]

-- error internal (int/inf int)
cerr1b :: ComponentInstance
cerr1b = ComponentInstance n1 $ BasicComponent nameC1 sig beh
 where
  sig = Signature [a]
                  []
                  (fromList [(a, m1)])
                  (fromList [(a, Nothing)])
  beh = LabelledTransitionSystem
    n1
    [EventLabel . CReceive $ a, InternalLabel (TimeValue 2) (TimeValue 4), InternalLabel (TimeValue 0) InfiniteValue]
    [State "0", State "1", State "2"]
    (State "0")
    [State "1", State "2"]
    [ Transition (State "0") (InternalLabel (TimeValue 2) (TimeValue 4)) (State "1")
    , Transition (State "0") (InternalLabel (TimeValue 0) InfiniteValue) (State "2")
    ]

-- error timeout (2 timeouts)
cerr2 :: ComponentInstance
cerr2 = ComponentInstance n1 $ BasicComponent nameC1 sig beh
 where
  sig = Signature [a]
                  []
                  (fromList [(a, m1)])
                  (fromList [(a, Nothing)])
  beh = LabelledTransitionSystem
    n1
    [EventLabel . CReceive $ a, TimeoutLabel 15, TimeoutLabel 10]
    [State "0", State "1", State "2", State "3"]
    (State "0")
    [State "2", State "3"]
    [ Transition (State "0") (EventLabel . CReceive $a) (State "1")
    , Transition (State "1") (TimeoutLabel 15) (State "2")
    , Transition (State "1") (TimeoutLabel 10) (State "3")
    ]

-- error timeout (other than reception)
cerr3 :: ComponentInstance
cerr3 = ComponentInstance n1 $ BasicComponent nameC1 sig beh
 where
  sig = Signature [a]
                  [b]
                  (fromList [(a, m1),(b,m1)])
                  (fromList [(a, Nothing),(b, Nothing)])
  beh = LabelledTransitionSystem
    n1
    [EventLabel . CReceive $ a, TimeoutLabel 15, EventLabel . CInvoke $ b]
    [State "0", State "1", State "2", State "3"]
    (State "0")
    [State "2", State "3"]
    [ Transition (State "0") (EventLabel . CReceive $ a) (State "1")
    , Transition (State "1") (TimeoutLabel 15) (State "2")
    , Transition (State "1") (EventLabel . CInvoke $ b) (State "3")
    ]

-- error time step
cerr4 :: ComponentInstance
cerr4 = ComponentInstance n1 $ BasicComponent nameC1 sig beh
 where
  sig = Signature [a]
                  []
                  (fromList [(a, m1)])
                  (fromList [(a, Nothing)])
  beh = LabelledTransitionSystem
    n1
    [EventLabel . CReceive $ a, InternalLabel (TimeValue 2) (TimeValue 4), InternalLabel (TimeValue 2) InfiniteValue]
    [State "0", State "1", State "2"]
    (State "0")
    [State "1", State "2"]
    [ Transition (State "0") (InternalLabel (TimeValue 2) (TimeValue 4)) (State "1")
    , Transition (State "0") (InternalLabel (TimeValue 2) InfiniteValue) (State "2")
    ]

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
