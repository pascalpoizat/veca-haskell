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
import           Data.Set         as S (fromList)
import           Data.Map         as M (fromList)
import           Trees.Tree       as IUT
import           Trees.Trifunctor
import Models.Name

dataProvider1 :: String -> Tree Int String String
dataProvider1 x = case x of
  "t1"  -> Leaf 1
  "t1'" -> Leaf 2
  "t2"  -> Node "T1" [("a", Leaf 1), ("b", Leaf 2)]
  "t3"  -> Node
    "T1"
    [ ("a", Node "T2" [("c", Leaf 1), ("d", Leaf 2)])
    , ("b", Node "T3" [("e", Leaf 3)])
    ]
  "t4" ->
    Node "T1" [("a", Node "T2" [("c", Leaf 1), ("d", Leaf 2)]), ("b", Leaf 3)]
  "t5" -> Node
    "T1"
    [ ("a", Node "T2" [("a", Leaf 4)])
    , ("b", Leaf 6)
    , ("c", Node "T2" [("a", Leaf 8)])
    , ("d", Leaf 10)
    ]
  "t6" -> Node
    "T1"
    [ ("a", Node "T2" [("a", Leaf 4), ("c", Node "T4" [("a", Leaf 2)])])
    , ("b", Leaf 6)
    , ("a", Node "T3" [("a", Leaf 8)])
    , ("d", Leaf 10)
    ]
  _ -> Leaf 0

dataProvider2 :: String -> Tree String String String
dataProvider2 x = case x of
  "t1" -> Leaf "*"
  "t2" -> Node "T1" [("a", Leaf "*"), ("b", Leaf "**")]
  "t3" -> Node
    "T1"
    [ ("a", Node "T2" [("c", Leaf "*"), ("d", Leaf "**")])
    , ("b", Node "T3" [("e", Leaf "***")])
    ]
  _ -> Leaf "*"

dataProvider3 :: String -> Tree Int String String
dataProvider3 x = case x of
  "t1" -> Leaf 1
  "t2" -> Node "T=T1" [("a", Leaf 1), ("b", Leaf 2)]
  "t3" -> Node
    "T=T1"
    [ ("a", Node "T=T2" [("c", Leaf 1), ("d", Leaf 2)])
    , ("b", Node "T=T3" [("e", Leaf 3)])
    ]
  _ -> Leaf 0

dataProvider4 :: String -> Tree String String String
dataProvider4 x
  | x == "t1" = Leaf "*"
  | x == "t2" = Node "T=T1" [("a", Leaf "*"), ("b", Leaf "**")]
  | x == "t3" = Node
    "T=T1"
    [ ("a", Node "T=T2" [("c", Leaf "*"), ("d", Leaf "**")])
    , ("b", Node "T=T3" [("e", Leaf "***")])
    ]
  | otherwise = Leaf "*"

treeTests :: TestTree
treeTests = testGroup "Tests" [properties, unittests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" []

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" []

unittests :: TestTree
unittests = testGroup
  "Unit tests"
  [ uIsValidTree
  , uEq
  , uOrd
  , uShow
  , uTrimap
  , uDirectSubtrees
  , uDirectSubtreesFor
  , uDirectSubtreesSuchThat
  , uLeafValues
  , uLeafValueMap
  , uNodeValues
  , uDepth
  ]

uEq :: TestTree
uEq = testGroup
  "Unit tests for Eq"
  [ testCase "== leaf / leaf (same)"
  $   dataProvider1 "t1"
  ==  dataProvider1 "t1"
  @?= True
  , testCase "== leaf / leaf (different)"
  $   dataProvider1 "t1"
  ==  dataProvider1 "t1'"
  @?= False
  , testCase "== leaf / node"
  $   dataProvider1 "t1"
  ==  dataProvider1 "t2"
  @?= False
  , testCase "== node / leaf"
  $   dataProvider1 "t2"
  ==  dataProvider1 "t1"
  @?= False
  , testCase "== node / node (same)"
  $   dataProvider1 "t2"
  ==  Node "T1" [("a", Leaf 1), ("b", Leaf 2)]
  @?= True
  , testCase "== node / node (same, distinct order of children)"
  $   dataProvider1 "t2"
  ==  Node "T1" [("b", Leaf 2), ("a", Leaf 1)]
  @?= True
  , testCase "== node / node (different, swap of children)"
  $   dataProvider1 "t2"
  ==  Node "T1" [("b", Leaf 1), ("a", Leaf 2)]
  @?= False
  ]

uOrd :: TestTree
uOrd = testGroup
  "Unit tests for Ord"
  [ testCase "comparison between two leaves (l1<l2)"
  $         dataProvider1 "t1"
  `compare` dataProvider1 "t1'"
  @?=       LT
  , testCase "comparison leaf / node"
  $         dataProvider1 "t1"
  `compare` dataProvider1 "t2"
  @?=       LT
  , testCase "comparison node / leaf"
  $         dataProvider1 "t2"
  `compare` dataProvider1 "t1"
  @?=       GT
  ]

uShow :: TestTree
uShow = testGroup
  "Unit tests for Show"
  [ testCase "show for a leaf" $ show (dataProvider1 "t1") == "Leaf 1" @?= True
  , testCase "show for a node with leaf children"
  $   show (Node 1 [(2, Leaf 22), (3, Leaf 33)])
  ==  "Node 1 [(2,Leaf 22),(3,Leaf 33)]"
  @?= True
  , testCase "show for a node with leaf children (different order of children)"
  $   show (Node 1 [(3, Leaf 33), (2, Leaf 22)])
  ==  "Node 1 [(3,Leaf 33),(2,Leaf 22)]"
  @?= True
  , testCase "show for a node with at least a node child"
  $   show (Node 1 [(2, Node 22 [(222, Leaf 2222)]), (3, Leaf 33)])
  ==  "Node 1 [(2,Node 22 [(222,Leaf 2222)]),(3,Leaf 33)]"
  @?= True
  , testCase "show for a node with no children (invalid tree)"
  $   show (Node 1 [] :: Tree Int Int Int)
  ==  "Node 1 []"
  @?= True
  ]

uIsValidTree :: TestTree
uIsValidTree = testGroup
  "Unit tests for isValidTree"
  [ testCase "isValidTree on a leaf" $ isValidTree (dataProvider1 "t1") @?= True
  , testCase "isValidTree on a node with subtrees"
  $   isValidTree (dataProvider1 "t2")
  @?= True
  , testCase "isValidTree on a node without subtree"
  $   isValidTree (Node 1 [])
  @?= False
  ]

type ISSTree = Tree Int String String
type SSSTree = Tree String String String

uTrimap :: TestTree
uTrimap = testGroup
  "Unit tests for trimap"
  [ testCase "trimap with id,id,id on leaves on a tree of depth 1"
  $   mytrimap1 id id id (fst t1)
  @?= snd t1
  , testCase "trimap with id,id,id on leaves on a tree of depth 2"
  $   trimap id id id (fst t2)
  @?= snd t2
  , testCase "trimap with id,id,id on leaves on a tree of depth 3"
  $   trimap id id id (fst t3)
  @?= snd t3
  , testCase "trimap with f,id,id on leaves on a tree of depth 1"
  $   mytrimap2 f id id (fst t1')
  @?= snd t1'
  , testCase "trimap with f,id,id on leaves on a tree of depth 2"
  $   trimap f id id (fst t2')
  @?= snd t2'
  , testCase "trimap with f,id,id on leaves on a tree of depth 3"
  $   trimap f id id (fst t3')
  @?= snd t3'
  , testCase "trimap with id,g,id on leaves on a tree of depth 1"
  $   mytrimap3 id g id (fst t1'')
  @?= snd t1''
  , testCase "trimap with id,g,id on leaves on a tree of depth 2"
  $   trimap id g id (fst t2'')
  @?= snd t2''
  , testCase "trimap with id,g,id on leaves on a tree of depth 3"
  $   trimap id g id (fst t3'')
  @?= snd t3''
  , testCase "trimap with f,g,id on a tree of depth 1"
  $   mytrimap4 f g id (fst t1''')
  @?= snd t1'''
  , testCase "trimap with f,g,id on a tree of depth 2"
  $   trimap f g id (fst t2''')
  @?= snd t2'''
  , testCase "trimap with f,g,id on a tree of depth 3"
  $   trimap f g id (fst t3''')
  @?= snd t3'''
  ]
 where
  f x = replicate x '*'
  g x = "T=" <> x
  h x = "C=" <> x
  t1    = (dataProvider1 "t1", dataProvider1 "t1")
  t2    = (dataProvider1 "t2", dataProvider1 "t2")
  t3    = (dataProvider1 "t3", dataProvider1 "t3")
  t1'   = (dataProvider1 "t1", dataProvider2 "t1")
  t2'   = (dataProvider1 "t2", dataProvider2 "t2")
  t3'   = (dataProvider1 "t3", dataProvider2 "t3")
  t1''  = (dataProvider1 "t1", dataProvider3 "t1")
  t2''  = (dataProvider1 "t2", dataProvider3 "t2")
  t3''  = (dataProvider1 "t3", dataProvider3 "t3")
  t1''' = (dataProvider1 "t1", dataProvider4 "t1")
  t2''' = (dataProvider1 "t2", dataProvider4 "t2")
  t3''' = (dataProvider1 "t3", dataProvider4 "t3")
  mytrimap1 =
    trimap :: (Int -> Int)
      -> (String -> String)
      -> (String -> String)
      -> ISSTree
      -> ISSTree
  mytrimap2 =
    trimap :: (Int -> String)
      -> (String -> String)
      -> (String -> String)
      -> ISSTree
      -> SSSTree
  mytrimap3 =
    trimap :: (Int -> Int)
      -> (String -> String)
      -> (String -> String)
      -> ISSTree
      -> ISSTree
  mytrimap4 =
    trimap :: (Int -> String)
      -> (String -> String)
      -> (String -> String)
      -> ISSTree
      -> SSSTree

uDirectSubtrees :: TestTree
uDirectSubtrees = testGroup
  "Unit tests for directSubtrees"
  [ testCase "tree of depth 1" $ directSubtrees (dataProvider1 "t1") @?= []
  , testCase "tree of depth 2"
  $   directSubtrees (dataProvider1 "t2")
  @?= [Leaf 1, Leaf 2]
  , testCase "balanced tree of depth 3"
  $   directSubtrees (dataProvider1 "t3")
  @?= [Node "T2" [("c", Leaf 1), ("d", Leaf 2)], Node "T3" [("e", Leaf 3)]]
  , testCase "unbalanced tree of depth 3"
  $   directSubtrees (dataProvider1 "t4")
  @?= [Node "T2" [("c", Leaf 1), ("d", Leaf 2)], Leaf 3]
  ]

uDirectSubtreesFor :: TestTree
uDirectSubtreesFor = testGroup
  "Unit tests for directSubtreesFor"
  [ testCase "leaf" $ directSubtreesFor "_" (dataProvider1 "t1") @?= []
  , testCase "unknown index" $ directSubtreesFor "_" (dataProvider1 "t2") @?= []
  , testCase "known index yielding unique subtree"
  $   directSubtreesFor "a" (dataProvider1 "t2")
  @?= [Leaf 1]
  , testCase "known index yielding several subtrees"
  $   S.fromList (directSubtreesFor "a" (dataProvider1 "t6"))
  @?= S.fromList
        [ Node "T3" [("a", Leaf 8)]
        , Node "T2" [("a", Leaf 4), ("c", Node "T4" [("a", Leaf 2)])]
        ]
  ]

uDirectSubtreesSuchThat :: TestTree
uDirectSubtreesSuchThat = testGroup "Unit tests for directSubtreesSuchThat" []

uLeafValues :: TestTree
uLeafValues = testGroup
  "Unit tests for leafValues"
  [ testCase "tree of depth 1" $ leafValues (dataProvider1 "t1") @?= [1]
  , testCase "tree of depth 2" $ leafValues (dataProvider1 "t2") @?= [1, 2]
  , testCase "balanced tree of depth 3"
  $   leafValues (dataProvider1 "t3")
  @?= [1, 2, 3]
  , testCase "unbalanced tree of depth 3"
  $   leafValues (dataProvider1 "t4")
  @?= [1, 2, 3]
  ]

uLeafValueMap :: TestTree
uLeafValueMap = testGroup
  "Unit tests for leafValues"
  [ testCase "tree of depth 1"
  $   leafValueMap (dataProvider1 "t1")
  @?= M.fromList [(Name [], 1)]
  , testCase "tree of depth 2"
  $   leafValueMap (dataProvider1 "t2")
  @?= M.fromList [(Name ["a"], 1), (Name ["b"], 2)]
  , testCase "balanced tree of depth 3"
  $   leafValueMap (dataProvider1 "t3")
  @?= M.fromList
        [(Name ["a", "c"], 1), (Name ["a", "d"], 2), (Name ["b", "e"], 3)]
  , testCase "unbalanced tree of depth 3"
  $   leafValueMap (dataProvider1 "t4")
  @?= M.fromList [(Name ["a", "c"], 1), (Name ["a", "d"], 2), (Name ["b"], 3)]
  ]

uNodeValues :: TestTree
uNodeValues = testGroup
  "Unit tests for nodeValues"
  [ testCase "tree of depth 1" $ nodeValues (dataProvider1 "t1") @?= []
  , testCase "tree of depth 2" $ nodeValues (dataProvider1 "t2") @?= ["T1"]
  , testCase "balanced tree of depth 3"
  $   nodeValues (dataProvider1 "t3")
  @?= ["T1", "T2", "T3"]
  , testCase "unbalanced tree of depth 3"
  $   nodeValues (dataProvider1 "t4")
  @?= ["T1", "T2"]
  , testCase "tree of depth 3 with more than 2 subtrees"
  $   nodeValues (dataProvider1 "t5")
  @?= ["T1", "T2", "T2"]
  , testCase "tree of depth 4 with more than 2 subtrees and a duplicate index"
  $   nodeValues (dataProvider1 "t6")
  @?= ["T1", "T2", "T4", "T3"]
  ]

uDepth :: TestTree
uDepth = testGroup
  "Unit tests for depth"
  [ testCase "tree with of depth 1" $ depth (dataProvider1 "t1") @?= 1
  , testCase "tree with of depth 2" $ depth (dataProvider1 "t2") @?= 2
  , testCase "balanced tree of depth 3" $ depth (dataProvider1 "t3") @?= 3
  , testCase "unbalanced tree of depth 3" $ depth (dataProvider1 "t4") @?= 3
  , testCase "tree of depth 3 with more than 2 subtrees"
  $   depth (dataProvider1 "t5")
  @?= 3
  , testCase "tree of depth 4 with more than 2 subtrees and a duplicate index"
  $   depth (dataProvider1 "t6")
  @?= 4
  ]
