-----------------------------------------------------------------------------
-- |
-- Module      :  TreeTests
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Test file for the Tree module.
-----------------------------------------------------------------------------

module TreeTests (treeTests)
where

import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC

import           Data.Monoid      as DM ((<>))
import           Tree             as IUT
import           Trifunctor

dataProvider1 :: String -> Tree Int String String
dataProvider1 x
  | x == "t1" = Leaf 1
  | x == "t2" = Node "T1" [("a",Leaf 1),("b",Leaf 2)]
  | x == "t3" =
    Node "T1"
         [("a",Node "T2" [("c",Leaf 1),("d",Leaf 2)])
         ,("b",Node "T3" [("e",Leaf 3)])]
  | x == "t4" =
    Node "T1" [("a",Node "T2" [("c",Leaf 1),("d",Leaf 2)]),("b",Leaf 3)]
  | x == "t5" =
    Node "T1"
         [("a",Node "T2" [("a",Leaf 4)])
         ,("b",Leaf 6)
         ,("c",Node "T2" [("a",Leaf 8)])
         ,("d",Leaf 10)]
  | x == "t6" =
    Node "T1"
         [("a",Node "T2" [("a",Leaf 4),("c",Node "T4" [("a",Leaf 2)])])
         ,("b",Leaf 6)
         ,("a",Node "T3" [("a",Leaf 8)])
         ,("d",Leaf 10)]
  | otherwise = Leaf 0
dataProvider2 :: String -> Tree String String String
dataProvider2 x
  | x == "t1" = Leaf "*"
  | x == "t2" = Node "T1" [("a",Leaf "*"),("b",Leaf "**")]
  | x == "t3" =
    Node "T1"
         [("a",Node "T2" [("c",Leaf "*"),("d",Leaf "**")])
         ,("b",Node "T3" [("e",Leaf "***")])]
  | otherwise = Leaf "*"
dataProvider3 :: String -> Tree Int String String
dataProvider3 x
  | x == "t1" = Leaf 1
  | x == "t2" = Node "T=T1" [("a",Leaf 1),("b",Leaf 2)]
  | x == "t3" =
    Node "T=T1"
         [("a",Node "T=T2" [("c",Leaf 1),("d",Leaf 2)])
         ,("b",Node "T=T3" [("e",Leaf 3)])]
  | otherwise = Leaf 0
dataProvider4 :: String -> Tree String String String
dataProvider4 x
  | x == "t1" = Leaf "*"
  | x == "t2" = Node "T=T1" [("a",Leaf "*"),("b",Leaf "**")]
  | x == "t3" =
    Node "T=T1"
         [("a",Node "T=T2" [("c",Leaf "*"),("d",Leaf "**")])
         ,("b",Node "T=T3" [("e",Leaf "***")])]
  | otherwise = Leaf "*"

treeTests :: TestTree
treeTests = testGroup "Tests" [properties,unittests]

properties :: TestTree
properties = testGroup "Properties" [scProps,qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" []

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" []

unittests :: TestTree
unittests =
  testGroup "Unit tests"
            [uIsValidTree
            ,uTrimap
            ,uDirectSubtrees
            ,uDirectSubtreesFor
            ,uDirectSubtreesSuchThat
            ,uLeafValues
            ,uNodeValues
            ,uDepth]

uIsValidTree :: TestTree
uIsValidTree =
  testGroup "Unit tests for isValidTree"
            [(testCase "isValidTree on a leaf" $
              isValidTree (dataProvider1 "t1") @?= True)
            ,(testCase "isValidTree on a node with subtrees" $
              isValidTree (dataProvider1 "t2") @?= True)
            ,(testCase "isValidTree on a node without subtree" $
              isValidTree (Node 1 []) @?= False)]

uTrimap :: TestTree
uTrimap =
  testGroup "Unit tests for trimap"
            [testCase "trimap with id,id,id on leaves on a tree of depth 1" $
             mytrimap1 id id id (fst t1) @?= snd t1
            ,testCase "trimap with id,id,id on leaves on a tree of depth 2" $
             trimap id id id (fst t2) @?= snd t2
            ,testCase "trimap with id,id,id on leaves on a tree of depth 3" $
             trimap id id id (fst t3) @?= snd t3
            ,testCase "trimap with f,id,id on leaves on a tree of depth 1" $
             mytrimap2 f id id (fst t1') @?= snd t1'
            ,testCase "trimap with f,id,id on leaves on a tree of depth 2" $
             trimap f id id (fst t2') @?= snd t2'
            ,testCase "trimap with f,id,id on leaves on a tree of depth 3" $
             trimap f id id (fst t3') @?= snd t3'
            ,testCase "trimap with id,g,id on leaves on a tree of depth 1" $
             mytrimap3 id g id (fst t1'') @?= snd t1''
            ,testCase "trimap with id,g,id on leaves on a tree of depth 2" $
             trimap id g id (fst t2'') @?= snd t2''
            ,testCase "trimap with id,g,id on leaves on a tree of depth 3" $
             trimap id g id (fst t3'') @?= snd t3''
            ,testCase "trimap with f,g,id on a tree of depth 1" $
             mytrimap4 f g id (fst t1''') @?= snd t1'''
            ,testCase "trimap with f,g,id on a tree of depth 2" $
             trimap f g id (fst t2''') @?= snd t2'''
            ,testCase "trimap with f,g,id on a tree of depth 3" $
             trimap f g id (fst t3''') @?= snd t3''']
  where f x = replicate x '*'
        g x = "T=" <> x
        h x = "C=" <> x
        t1 = (dataProvider1 "t1",dataProvider1 "t1")
        t2 = (dataProvider1 "t2",dataProvider1 "t2")
        t3 = (dataProvider1 "t3",dataProvider1 "t3")
        t1' = (dataProvider1 "t1",dataProvider2 "t1")
        t2' = (dataProvider1 "t2",dataProvider2 "t2")
        t3' = (dataProvider1 "t3",dataProvider2 "t3")
        t1'' = (dataProvider1 "t1",dataProvider3 "t1")
        t2'' = (dataProvider1 "t2",dataProvider3 "t2")
        t3'' = (dataProvider1 "t3",dataProvider3 "t3")
        t1''' = (dataProvider1 "t1",dataProvider4 "t1")
        t2''' = (dataProvider1 "t2",dataProvider4 "t2")
        t3''' = (dataProvider1 "t3",dataProvider4 "t3")
        mytrimap1 =
          trimap :: (Int -> Int) -> (String -> String) -> (String -> String) -> Tree Int String String -> Tree Int String String
        mytrimap2 =
          trimap :: (Int -> String) -> (String -> String) -> (String -> String) -> Tree Int String String -> Tree String String String
        mytrimap3 =
          trimap :: (Int -> Int) -> (String -> String) -> (String -> String) -> Tree Int String String -> Tree Int String String
        mytrimap4 =
          trimap :: (Int -> String) -> (String -> String) -> (String -> String) -> Tree Int String String -> Tree String String String

uDirectSubtrees :: TestTree
uDirectSubtrees =
  testGroup "Unit tests for directSubtrees"
            [(testCase "tree of depth 1" $
              directSubtrees (dataProvider1 "t1") @?= [])
            ,(testCase "tree of depth 2" $
              directSubtrees (dataProvider1 "t2") @?= [Leaf 1,Leaf 2])
            ,(testCase "balanced tree of depth 3" $
              directSubtrees (dataProvider1 "t3") @?=
              [Node "T2" [("c",Leaf 1),("d",Leaf 2)],Node "T3" [("e",Leaf 3)]])
            ,(testCase "unbalanced tree of depth 3" $
              directSubtrees (dataProvider1 "t4") @?=
              [Node "T2" [("c",Leaf 1),("d",Leaf 2)],Leaf 3])]

uDirectSubtreesFor :: TestTree
uDirectSubtreesFor = testGroup "Unit tests for directSubtreesFor" []

uDirectSubtreesSuchThat :: TestTree
uDirectSubtreesSuchThat = testGroup "Unit tests for directSubtreesSuchThat" []

uLeafValues :: TestTree
uLeafValues =
  testGroup "Unit tests for leafValues"
            [(testCase "tree of depth 1" $
              leafValues (dataProvider1 "t1") @?= [1])
            ,(testCase "tree of depth 2" $
              leafValues (dataProvider1 "t2") @?= [1,2])
            ,(testCase "balanced tree of depth 3" $
              leafValues (dataProvider1 "t3") @?= [1,2,3])
            ,(testCase "unbalanced tree of depth 3" $
              leafValues (dataProvider1 "t4") @?= [1,2,3])]

uNodeValues :: TestTree
uNodeValues =
  testGroup "Unit tests for nodeValues"
            [(testCase "tree of depth 1" $
              nodeValues (dataProvider1 "t1") @?= [])
            ,(testCase "tree of depth 2" $
              nodeValues (dataProvider1 "t2") @?= ["T1"])
            ,(testCase "balanced tree of depth 3" $
              nodeValues (dataProvider1 "t3") @?= ["T1","T2","T3"])
            ,(testCase "unbalanced tree of depth 3" $
              nodeValues (dataProvider1 "t4") @?= ["T1","T2"])
            ,(testCase "tree of depth 3 with more than 2 subtrees" $
             nodeValues (dataProvider1 "t5") @?= ["T1","T2","T2"])
            ,(testCase "tree of depth 4 with more than 2 subtrees and a duplicate index" $
             nodeValues (dataProvider1 "t6") @?= ["T1","T2","T4","T3"])]

uDepth :: TestTree
uDepth =
  testGroup "Unit tests for depth"
            [(testCase "tree with of depth 1" $
              depth (dataProvider1 "t1") @?= 1)
            ,(testCase "tree with of depth 2" $
              depth (dataProvider1 "t2") @?= 2)
            ,(testCase "balanced tree of depth 3" $
              depth (dataProvider1 "t3") @?= 3)
            ,(testCase "unbalanced tree of depth 3" $
              depth (dataProvider1 "t4") @?= 3)
            ,(testCase "tree of depth 3 with more than 2 subtrees" $
             depth (dataProvider1 "t5") @?= 3)
            ,(testCase "tree of depth 4 with more than 2 subtrees and a duplicate index" $
             depth (dataProvider1 "t6") @?= 4)]
