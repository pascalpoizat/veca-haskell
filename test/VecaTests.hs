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

import           Data.Map.Strict                 (fromList)
import           Models.Events
import           Models.LabelledTransitionSystem
import           Models.Name
import           Models.TimedAutomaton
import           Trees.Tree
import           Veca.Veca

vecaTests :: TestTree
vecaTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests for the Veca module"
            [uCToTA
            ,uCToCTree
            ,uCTreeToTATree
            ,uFlatten]

uCToTA :: TestTree
uCToTA =
  testGroup "Unit tests for cToTA"
            [testCase "case 2.1" $ cToTA c1 @?= ta1
            ,testCase "case 2.2" $ cToTA c2 @?= ta2
            ,testCase "case 2.3" $ cToTA c3 @?= ta3
            ,testCase "case 2.4" $ cToTA c4 @?= ta4]

uCToCTree :: TestTree
uCToCTree =
  testGroup "Unit tests for cToCTree"
            [testCase "case 2.7 (common names in subtrees)" $ cToCTree c7 @?= tree1]

uCTreeToTATree :: TestTree
uCTreeToTATree =
  testGroup "Unit tests for cTreeToTATree"
            [testCase "case 2.7 (common names in subtrees)" $ cTreeToTATree tree1 @?= tree1']

uFlatten :: TestTree
uFlatten =
  testGroup "Unit tests for flatten"
            [testCase "case 2.7 (common names in subtrees)" $ flatten tree1' @?= tas1]

id1 :: Name
id1 = Name ["Model001"]

id2 :: Name
id2 = Name ["Model002"]

id3 :: Name
id3 = Name ["Model003"]

id4 :: Name
id4 = Name ["Model004"]

id5 :: Name
id5 = Name ["Model005"]

id6 :: Name
id6 = Name ["Model006"]

id7 :: Name
id7 = Name ["Model007"]

a :: Operation
a = Operation $ Name ["a"]

b :: Operation
b = Operation $ Name ["b"]

c :: Operation
c = Operation $ Name ["c"]

n1 :: Name
n1 = Name ["1"]

n2 :: Name
n2 = Name ["2"]

n3 :: Name
n3 = Name ["3"]

n4 :: Name
n4 = Name ["4"]

n5 :: Name
n5 = Name ["5"]

n6 :: Name
n6 = Name ["6"]

m1 :: Message
m1 = Message (Name ["m1"]) (MessageType "...")

ta1 :: VTA
ta1 =
  TimedAutomaton
    id1
    [Location "0", Location "1", Location "2"]
    (Location "0")
    []
    [CTau, CReceive a, CReceive c]
    [Edge (Location "0")
          (CReceive a)
          []
          []
          (Location "1")
    ,Edge (Location "1")
          (CReceive c)
          []
          []
          (Location "2")
    ,Edge (Location "2")
          CTau
          []
          []
          (Location "2")]
    []

ta2 :: VTA
ta2 =
  TimedAutomaton
    id2
    [Location "0", Location "1", Location "2"]
    (Location "0")
    []
    [CTau, CInvoke a, CReceive b]
    [Edge (Location "0")
          (CInvoke a)
          []
          []
          (Location "1")
    ,Edge (Location "1")
          (CReceive b)
          []
          []
          (Location "2")
    ,Edge (Location "2")
          CTau
          []
          []
          (Location "2")]
    []

ta3 :: VTA
ta3 =
  TimedAutomaton
    id3
    [Location "0", Location "1"]
    (Location "0")
    []
    [CTau, CReceive a]
    [Edge (Location "0")
          (CReceive a)
          []
          []
          (Location "1")
    ,Edge (Location "1")
          CTau
          []
          []
          (Location "1")]
    []

ta4 :: VTA
ta4 =
  TimedAutomaton
    id4
    [Location "0", Location "1", Location "2"]
    (Location "0")
    []
    [CTau, CInvoke a, CInvoke b]
    [Edge (Location "0")
          (CInvoke a)
          []
          []
          (Location "1")
    ,Edge (Location "1")
          (CInvoke b)
          []
          []
          (Location "2")
    ,Edge (Location "2")
          CTau
          []
          []
          (Location "2")]
    []

c1 :: Component
c1 = BasicComponent nameC1 sig beh tcs
  where
    nameC1 = id1
    sig = Signature
      [a,c]
      []
      (fromList [(a, m1),(c, m1)])
      (fromList [(a, Nothing),(c, Nothing)])
    beh = LabelledTransitionSystem
      [CReceive a, CReceive c]
      [State "0", State "1", State "2"]
      (State "0")
      [State "2"]
      [Transition (State "0") (CReceive a) (State "1")
      ,Transition (State "1") (CReceive c) (State "2")]
    tcs = []

c2 :: Component
c2 = BasicComponent nameC2 sig beh tcs
  where
    nameC2 = id2
    sig = Signature
      [b]
      [a]
      (fromList [(a, m1),(b, m1)])
      (fromList [(a, Nothing),(b, Nothing)])
    beh = LabelledTransitionSystem
      [CInvoke a, CReceive b]
      [State "0", State "1", State "2"]
      (State "0")
      [State "2"]
      [Transition (State "0") (CInvoke a) (State "1")
      ,Transition (State "1") (CReceive b) (State "2")]
    tcs = []

c3 :: Component
c3 = BasicComponent nameC3 sig beh tcs
  where
    nameC3 = id3
    sig = Signature
      [a]
      []
      (fromList [(a, m1)])
      (fromList [(a, Nothing)])
    beh = LabelledTransitionSystem
      [CReceive a]
      [State "0", State "1"]
      (State "0")
      [State "1"]
      [Transition (State "0") (CReceive a) (State "1")]
    tcs = []

c4 :: Component
c4 = BasicComponent nameC4 sig beh tcs
  where
    nameC4 = id4
    sig = Signature
      []
      [a,b]
      (fromList [(a, m1),(b, m1)])
      (fromList [(a, Nothing),(b, Nothing)])
    beh = LabelledTransitionSystem
      [CInvoke a, CInvoke b]
      [State "0", State "1", State "2"]
      (State "0")
      [State "2"]
      [Transition (State "0") (CInvoke a) (State "1")
      ,Transition (State "1") (CInvoke b) (State "2")]
    tcs = []

c5 :: Component
c5 = CompositeComponent nameC5 sig cs inb exb
  where
    nameC5 = id5
    sig = Signature
      [b,c]
      []
      (fromList [(b, m1),(c, m1)])
      (fromList [(b, Nothing),(c, Nothing)])
    cs = [(n1,c1),(n2,c2)]
    inb = [InternalBinding (JoinPoint n2 a) (JoinPoint n1 a)]
    exb = [ExternalBinding (JoinPoint self b) (JoinPoint n2 b)
          ,ExternalBinding (JoinPoint self c) (JoinPoint n1 c)]

c6 :: Component
c6 = CompositeComponent nameC6 sig cs inb exb
  where
    nameC6 = id6
    sig = Signature
      []
      [b]
      (fromList [(b, m1)])
      (fromList [(b, Nothing)])
    cs = [(n3,c3),(n4,c4)]
    inb = [InternalBinding (JoinPoint n4 a) (JoinPoint n3 a)]
    exb = [ExternalBinding (JoinPoint n4 b) (JoinPoint self b)]

c7 :: Component
c7 = CompositeComponent nameC7 sig cs inb exb
    where
      nameC7 = id7
      sig = Signature
        [c]
        []
        (fromList [(c, m1)])
        (fromList [(c, Nothing)])
      cs = [(n5,c5),(n6,c6)]
      inb = [InternalBinding (JoinPoint n6 b) (JoinPoint n5 b)]
      exb = [ExternalBinding (JoinPoint self c) (JoinPoint n5 c)]

tree1 :: VCTree
tree1 = Node c7 [(n5,st5),(n6,st6)]
      where
        st5 = Node c5 [(n1,st1),(n2,st2)]
        st6 = Node c6 [(n3,st3),(n4,st4)]
        st1 = Leaf c1
        st2 = Leaf c2
        st3 = Leaf c3
        st4 = Leaf c4

tree1' :: VTATree
tree1' = Node c7 [(n5,st5'),(n6,st6')]
      where
        st5' = Node c5 [(n1,st1'),(n2,st2')]
        st6' = Node c6 [(n3,st3'),(n4,st4')]
        st1' = Leaf ta1
        st2' = Leaf ta2
        st3' = Leaf ta3
        st4' = Leaf ta4

ta1' :: VTA
ta1' = relabel sub1 ta1
        where
          sub1 = [(CReceive a, CReceive $ indexBy id5 a)
                 ,(CReceive c, CReceive $ indexBy id7 c)]

ta2' :: VTA
ta2' = relabel sub2 ta2
        where
          sub2 = [(CInvoke a, CInvoke $ indexBy id5 a)
                 ,(CReceive b, CReceive $ indexBy id7 b)]

ta3' :: VTA
ta3' = relabel sub3 ta3
        where
          sub3 = [(CReceive a, CReceive $ indexBy id6 a)]

ta4' :: VTA
ta4' = relabel sub4 ta4
        where
          sub4 = [(CInvoke a, CInvoke $ indexBy id6 a)
                 ,(CInvoke b, CInvoke $ indexBy id7 b)]

tas1 :: [VTA]
tas1 = [ta1',ta2',ta3',ta4']
