import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import qualified Data.Set              as Set

import qualified Tree

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
unittests = testGroup "Unit tests" [treeleavesUnit, treedepthUnit]

treeleavesUnit =
  testGroup "Unit tests for treeleaves"
            [testCase "Set of leaves of a tree with depth 1" $
             Tree.treeleaves (Tree.Leaf 1) @?= Set.fromList [1]
            ,testCase "set of leaves of a tree with depth 2" $
             Tree.treeleaves (Tree.Node 1 [Tree.Leaf 1,Tree.Leaf 2]) @?= Set.fromList [1,2]
            ,testCase "set of leaves of a tree with depth 3" $
             Tree.treeleaves (Tree.Node 6 [Tree.Node 3 [Tree.Leaf 1, Tree.Leaf 2],Tree.Node 5 [Tree.Leaf 4]]) @?= Set.fromList [1,2,4]
            ]

treedepthUnit =
  testGroup "Unit tests for depth"
            [testCase "depth of a tree with depth 1" $
             Tree.treedepth (Tree.Leaf 1) @?= 1
            ,testCase "depth of a tree with depth 2" $
             Tree.treedepth (Tree.Node 1 [Tree.Leaf 1,Tree.Leaf 2]) @?= 2
            ,testCase "depth of a balanced tree with depth 3" $
             Tree.treedepth (Tree.Node 6 [Tree.Node 3 [Tree.Leaf 1, Tree.Leaf 2],Tree.Node 5 [Tree.Leaf 4]]) @?= 3
            ,testCase "depth of an unbalanced tree with depth 3" $
             Tree.treedepth (Tree.Node 6 [Tree.Node 3 [Tree.Leaf 1, Tree.Leaf 2], Tree.Leaf 4]) @?= 3
            ]
