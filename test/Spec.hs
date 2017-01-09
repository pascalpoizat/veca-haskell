-- tests
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
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
unittests = testGroup "Unit tests" [u_bimap, u_leaves, u_depth]

u_bimap =
  testGroup "Unit tests for bimap"
            [testCase "Bimap with id on leaves on a tree of depth 1" $
             mybimap4 id id t1 @?= id t1
            , testCase "Bimap with id on leaves on a tree of depth 2" $
             bimap id id t2 @?= id t2
            , testCase "Bimap with id on leaves on a tree of depth 3" $
             bimap id id t3 @?= id t3
            , testCase "Bimap with id on leaves on a tree of depth 1" $
             mybimap3 f id t1 @?= Leaf "*"
            , testCase "Bimap with id on leaves on a tree of depth 2" $
             bimap f id t2 @?= Node "a" [Leaf "**",Leaf "***"]
            , testCase "Bimap with id on leaves on a tree of depth 3" $
             bimap f id t3 @?= Node "a" [Node "b" [Leaf "*",Leaf "**"],Node "c" [Leaf "****"]]
            , testCase "Bimap with id on leaves on a tree of depth 1" $
             mybimap2 id g t1 @?= Leaf 1
            , testCase "Bimap with id on leaves on a tree of depth 2" $
             bimap id g t2 @?= Node "aa" [Leaf 2,Leaf 3]
            , testCase "Bimap with id on leaves on a tree of depth 3" $
             bimap id g t3 @?= Node "aa" [Node "bb" [Leaf 1,Leaf 2],Node "cc" [Leaf 4]]
            , testCase "Bimap with no id on a tree of depth 1" $
             mybimap1 f g t1 @?= Leaf "*"
            ,testCase "Bimap with no id on a tree of depth 2" $
             bimap f g t2 @?= Node "aa" [Leaf "**",Leaf "***"]
            ,testCase "Bimap with no id on a tree of depth 3" $
             bimap f g t3 @?=
             Node "aa" [Node "bb" [Leaf "*",Leaf "**"],Node "cc" [Leaf "****"]]]
  where f x = take x (repeat '*')
        g x = x ++ x
        t1 = Leaf 1
        t2 = Node "a" [Leaf 2,Leaf 3]
        t3 = Node "a" [Node "b" [Leaf 1,Leaf 2],Node "c" [Leaf 4]]
        mybimap1 = bimap::(Int->String)->(String->String)->(Tree Int String)->(Tree String String)
        mybimap2 = bimap::(Int->Int)->(String->String)->(Tree Int String)->(Tree Int String)
        mybimap3 = bimap::(Int->String)->(String->String)->(Tree Int String)->(Tree String String)
        mybimap4 = bimap::(Int->Int)->(String->String)->(Tree Int String)->(Tree Int String)

u_leaves =
  testGroup "Unit tests for leaves"
            [testCase "Set of leaves of a tree of depth 1" $
             leaves t1 @?= fromList [1]
            ,testCase "set of leaves of a tree of depth 2" $
             leaves t2 @?= fromList [2,3]
            ,testCase "set of leaves of a tree of depth 3" $
             leaves t3 @?= fromList [1,2,4]]
  where t1 = Leaf 1
        t2 = Node "a" [Leaf 2,Leaf 3]
        t3 = Node "a" [Node "b" [Leaf 1,Leaf 2],Node "c" [Leaf 4]]

u_depth =
  testGroup "Unit tests for depth"
            [testCase "depth of a tree with of 1" $ depth t1 @?= 1
            ,testCase "depth of a tree with of 2" $ depth t2 @?= 2
            ,testCase "depth of a balanced tree of depth 3" $ depth t3 @?= 3
            ,testCase "depth of an unbalanced tree of depth 3" $ depth t4 @?= 3]
  where t1 = Leaf 1
        t2 = Node "a" [Leaf 2,Leaf 3]
        t3 = Node "a" [Node "b" [Leaf 1,Leaf 2],Node "c" [Leaf 4]]
        t4 = Node "a" [Node "b" [Leaf 1,Leaf 2],Leaf 4]
