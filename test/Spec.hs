-- tests
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Data.Set              as S (fromList)

import           Tree

main :: IO ()
main = defaultMain tests

dataProvider1 :: String -> Tree Int String String
dataProvider1 x
  | x=="t1" = Leaf 1
  | x=="t2" = Node "T1" [("a",Leaf 1),("b",Leaf 2)]
  | x=="t3" = Node "T1" [("a",Node "T2" [("c",Leaf 1),("d",Leaf 2)]),("b", Node "T3" [("e",Leaf 3)])]
  | x=="t4" = Node "T1" [("a",Node "T2" [("c",Leaf 1),("d",Leaf 2)]), ("b", Leaf 3)]
  | otherwise = Leaf 0
dataProvider2 :: String -> Tree String String String
dataProvider2 x
  | x=="t1" = Leaf "*"
  | x=="t2" = Node "T1" [("a",Leaf "*"),("b",Leaf "**")]
  | x=="t3" = Node "T1" [("a",Node "T2" [("c",Leaf "*"),("d",Leaf "**")]),("b", Node "T3" [("e",Leaf "***")])]
  | otherwise = Leaf "*"
dataProvider3 :: String -> Tree Int String String
dataProvider3 x
  | x=="t1" = Leaf 1
  | x=="t2" = Node "T=T1" [("a",Leaf 1),("b",Leaf 2)]
  | x=="t3" = Node "T=T1" [("a",Node "T=T2" [("c",Leaf 1),("d",Leaf 2)]),("b", Node "T=T3" [("e",Leaf 3)])]
  | otherwise = Leaf 0
dataProvider4 :: String -> Tree String String String
dataProvider4 x
  | x=="t1" = Leaf "*"
  | x=="t2" = Node "T=T1" [("a",Leaf "*"),("b",Leaf "**")]
  | x=="t3" = Node "T=T1" [("a",Node "T=T2" [("c",Leaf "*"),("d",Leaf "**")]),("b", Node "T=T3" [("e",Leaf "***")])]
  | otherwise = Leaf "*"

tests :: TestTree
tests = testGroup "Tests" [properties, unittests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  []

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  []

unittests :: TestTree
unittests = testGroup "Unit tests" [u_trimap, u_leaves, u_depth]

u_trimap :: TestTree
u_trimap =
  testGroup "Unit tests for trimap"
            [testCase "trimap with id,id,id on leaves on a tree of depth 1" $
             mytrimap1 id id id (fst t1) @?= id (snd t1)
            ,testCase "trimap with id,id,id on leaves on a tree of depth 2" $
             trimap id id id (fst t2) @?= id (snd t2)
            ,testCase "trimap with id,id,id on leaves on a tree of depth 3" $
             trimap id id id (fst t3) @?= id (snd t3)
            ,testCase "trimap with f,id,id on leaves on a tree of depth 1" $
             mytrimap2 f id id (fst t1') @?= (snd t1')
            ,testCase "trimap with f,id,id on leaves on a tree of depth 2" $
             trimap f id id (fst t2') @?= (snd t2')
            ,testCase "trimap with f,id,id on leaves on a tree of depth 3" $
             trimap f id id (fst t3') @?= (snd t3')
            ,testCase "trimap with id,g,id on leaves on a tree of depth 1" $
             mytrimap3 id g id (fst t1'') @?= (snd t1'')
            ,testCase "trimap with id,g,id on leaves on a tree of depth 2" $
             trimap id g id (fst t2'') @?= (snd t2'')
            ,testCase "trimap with id,g,id on leaves on a tree of depth 3" $
             trimap id g id (fst t3'') @?= (snd t3'')
            ,testCase "trimap with f,g,id on a tree of depth 1" $
             mytrimap4 f g id (fst t1''') @?= (snd t1''')
            ,testCase "trimap with f,g,id on a tree of depth 2" $
             trimap f g id (fst t2''') @?= (snd t2''')
            ,testCase "trimap with f,g,id on a tree of depth 3" $
             trimap f g id (fst t3''') @?= (snd t3''')]
  where f x = take x (repeat '*')
        g x = "T=" ++ x
        h x = "C=" ++ x
        t1 = (dataProvider1 "t1", dataProvider1 "t1")
        t2 = (dataProvider1 "t2", dataProvider1 "t2")
        t3 = (dataProvider1 "t3", dataProvider1 "t3")
        t1' = (dataProvider1 "t1", dataProvider2 "t1")
        t2' = (dataProvider1 "t2", dataProvider2 "t2")
        t3' = (dataProvider1 "t3", dataProvider2 "t3")
        t1'' = (dataProvider1 "t1", dataProvider3 "t1")
        t2'' = (dataProvider1 "t2", dataProvider3 "t2")
        t3'' = (dataProvider1 "t3", dataProvider3 "t3")
        t1''' = (dataProvider1 "t1", dataProvider4 "t1")
        t2''' = (dataProvider1 "t2", dataProvider4 "t2")
        t3''' = (dataProvider1 "t3", dataProvider4 "t3")
        mytrimap1 =
          trimap :: (Int -> Int) -> (String -> String) -> (String -> String) -> (Tree Int String String) -> (Tree Int String String)
        mytrimap2 =
          trimap :: (Int -> String) -> (String -> String) -> (String -> String) -> (Tree Int String String) -> (Tree String String String)
        mytrimap3 =
          trimap :: (Int -> Int) -> (String -> String) -> (String -> String) -> (Tree Int String String) -> (Tree Int String String)
        mytrimap4 =
          trimap :: (Int -> String) -> (String -> String) -> (String -> String) -> (Tree Int String String) -> (Tree String String String)

u_leaves :: TestTree
u_leaves =
  testGroup "Unit tests for leaves"
          [(testCase "tree of depth 1" $ (leafValues (dataProvider1 "t1")) @?= (fromList [1]))
          ,(testCase "tree of depth 2" $ (leafValues (dataProvider1 "t2")) @?= (fromList [1,2]))
          ,(testCase "balanced tree of depth 3" $ (leafValues (dataProvider1 "t3")) @?= (fromList [1,2,3]))]

u_depth :: TestTree
u_depth =
  testGroup "Unit tests for depth"
          [(testCase "tree with of depth 1" $ (depth (dataProvider1 "t1")) @?= 1)
          ,(testCase "tree with of depth 2" $ (depth (dataProvider1 "t2")) @?= 2)
          ,(testCase "balanced tree of depth 3" $ (depth (dataProvider1 "t3")) @?= 3)
          ,(testCase "unbalanced tree of depth 3" $ (depth (dataProvider1 "t4")) @?= 3)]
