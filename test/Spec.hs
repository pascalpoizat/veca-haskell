import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Data.Set              as S

import           Tree

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unittests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  []

qcProps = testGroup "(checked by QuickCheck)"
  []

unittests :: TestTree
unittests = testGroup "Unit tests" [u_leaves, u_depth]

u_leaves =
  testGroup "Unit tests for leaves"
            [testCase "Set of leaves of a tree with depth 1" $
             leaves (Leaf 1) @?= fromList [1]
            ,testCase "set of leaves of a tree with depth 2" $
             leaves (Node 1 [Leaf 1, Leaf 2]) @?= fromList [1,2]
            ,testCase "set of leaves of a tree with depth 3" $
             leaves (Node 6 [Node 3 [Leaf 1, Leaf 2], Node 5 [Leaf 4]]) @?= fromList [1,2,4]
            ]

u_depth =
  testGroup "Unit tests for depth"
            [testCase "depth of a tree with depth 1" $
             depth (Leaf 1) @?= 1
            ,testCase "depth of a tree with depth 2" $
             depth (Node 1 [Leaf 1, Leaf 2]) @?= 2
            ,testCase "depth of a balanced tree with depth 3" $
             depth (Node 6 [Node 3 [Leaf 1, Leaf 2], Node 5 [Leaf 4]]) @?= 3
            ,testCase "depth of an unbalanced tree with depth 3" $
             depth (Node 6 [Node 3 [Leaf 1, Leaf 2], Leaf 4]) @?= 3
            ]
