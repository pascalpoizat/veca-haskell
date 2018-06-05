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

a :: Operation
a = Operation $ Name ["a"]

b :: Operation
b = Operation $ Name ["b"]

c :: Operation
c = Operation $ Name ["c"]

m1 :: Message
m1 = Message (Name ["m1"]) (MessageType "...")

ta1 :: VTA
ta1 =
  TimedAutomaton
    n1
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
    n2
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
    n3
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
    n4
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

n1 :: Name
n1 = Name ["c1"]
nameC1 :: Name
nameC1 = Name ["Type1"]
        
c1 :: ComponentInstance
c1 = ComponentInstance n1 $ BasicComponent nameC1 sig beh tcs
  where
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

n2 :: Name
n2 = Name ["c2"]
nameC2 :: Name
nameC2 = Name ["Type2"]

c2 :: ComponentInstance
c2 = ComponentInstance n2 $ BasicComponent nameC2 sig beh tcs
  where
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

n3 :: Name
n3 = Name ["c3"]
nameC3 :: Name
nameC3 = Name ["Type3"]

c3 :: ComponentInstance
c3 = ComponentInstance n3 $ BasicComponent nameC3 sig beh tcs
  where
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

n4 :: Name
n4 = Name ["c4"]
nameC4 :: Name
nameC4 = Name ["Type4"]

c4 :: ComponentInstance
c4 = ComponentInstance n4 $ BasicComponent nameC4 sig beh tcs
  where
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

n5 :: Name
n5 = Name ["c5"]

c5 :: ComponentInstance
c5 = ComponentInstance n5 $ CompositeComponent n5 sig cs inb exb
  where
    sig = Signature
      [b,c]
      []
      (fromList [(b, m1),(c, m1)])
      (fromList [(b, Nothing),(c, Nothing)])
    cs = [c1, c2]
    inb = [Binding Internal (JoinPoint n2 a) (JoinPoint n1 a)]
    exb = [Binding External (JoinPoint self b) (JoinPoint n2 b)
          ,Binding External (JoinPoint self c) (JoinPoint n1 c)]

n6 :: Name
n6 = Name ["c6"]

c6 :: ComponentInstance
c6 = ComponentInstance n6 $ CompositeComponent n6 sig cs inb exb
  where
    sig = Signature
      []
      [b]
      (fromList [(b, m1)])
      (fromList [(b, Nothing)])
    cs = [c3, c4]
    inb = [Binding Internal (JoinPoint n4 a) (JoinPoint n3 a)]
    exb = [Binding External (JoinPoint n4 b) (JoinPoint self b)]

n7 :: Name
n7 = Name ["c7"]

c7 :: ComponentInstance
c7 = ComponentInstance n7 $ CompositeComponent n7 sig cs inb exb
    where
      sig = Signature
        [c]
        []
        (fromList [(c, m1)])
        (fromList [(c, Nothing)])
      cs = [c5, c6]
      inb = [Binding Internal (JoinPoint n6 b) (JoinPoint n5 b)]
      exb = [Binding External (JoinPoint self c) (JoinPoint n5 c)]

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
ta1' =  prefix (n7 <> n5) $ 
        relabel sub1 ta1
        where
          sub1 = [(CReceive a, CReceive $ indexBy (n7 <> n5) a)
                 ,(CReceive c, CReceive $ indexBy n7 c)]

ta2' :: VTA
ta2' =  prefix (n7 <> n5) $ 
        relabel sub2 ta2
        where
          sub2 = [(CInvoke a, CInvoke $ indexBy (n7 <> n5) a)
                 ,(CReceive b, CReceive $ indexBy n7 b)]

ta3' :: VTA
ta3' =  prefix (n7 <> n6) $ 
        relabel sub3 ta3
        where
          sub3 = [(CReceive a, CReceive $ indexBy (n7 <> n6) a)]

ta4' :: VTA
ta4' =  prefix (n7 <> n6) $ 
        relabel sub4 ta4
        where
          sub4 = [(CInvoke a, CInvoke $ indexBy (n7 <> n6) a)
                 ,(CInvoke b, CInvoke $ indexBy n7 b)]

tas1 :: [VTA]
tas1 = [ta1',ta2',ta3',ta4']
