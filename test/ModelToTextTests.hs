-----------------------------------------------------------------------------
-- |
-- Module      :  ModelToTextTests
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Test file for the ModelToText module.
-----------------------------------------------------------------------------

module ModelToTextTests (modelToTextTests)
where

import           Data.Set
import           Test.Tasty
import           Test.Tasty.HUnit
import           Transformations.ModelToText
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC

modelToTextTests :: TestTree
modelToTextTests = testGroup "Tests" [properties,unittests]

properties :: TestTree
properties = testGroup "Properties" [scProps,qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" []

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" []

unittests :: TestTree
unittests =
  testGroup "Unit tests"
            [uFoldMapToString
            ,uFoldMapToString']
-- Lists
dataProvider1 :: [[Integer]]
dataProvider1 = [[], [2], [2,4,6,8]]

-- Sets
dataProvider2 :: [Set Integer]
dataProvider2 = fmap fromList dataProvider1

-- Functions
f1 :: Integer -> Integer
f1 x = x+1

f1' :: Integer -> String
f1' = show . f1

f2' :: Integer -> String
f2' x = "-" ++ f1' x ++ "-"

-- Unit Tests
uFoldMapToString :: TestTree
uFoldMapToString =
  testGroup "Unit tests for foldMapToString"
            [testCase "empty list" $
              foldMapToString "{"
                               ", "
                               "}"
                               f1'
                               (head dataProvider1) @?=
              ""
            ,testCase "list of length 1" $
              foldMapToString "{"
                               ", "
                               "}"
                               f1'
                               (dataProvider1 !! 1) @?=
              "{3}"
            ,testCase "list of length >1" $
              foldMapToString "{"
                               ", "
                               "}"
                               f1'
                               (dataProvider1 !! 2) @?=
              "{3, 5, 7, 9}"
            ,testCase "empty set" $
              foldMapToString "{\n"
                               "\n"
                               "\n}"
                               f2'
                               (head dataProvider2) @?=
              ""
            ,testCase "set of size 1" $
              foldMapToString "{\n"
                               "\n"
                               "\n}"
                               f2'
                               (dataProvider2 !! 1) @?=
              "{\n-3-\n}"
            ,testCase "set of size >1" $
              fromList (lines (foldMapToString "{\n"
                                               "\n"
                                               "\n}"
                                               f2'
                                               (dataProvider2 !! 2))) @?=
              fromList ["{","}","-3-","-7-","-5-","-9-"]]

uFoldMapToString' :: TestTree
uFoldMapToString' =
  testGroup "Unit tests for foldMapToString'"
            [testCase "empty list" $
              foldMapToString' ", "
                                f1'
                                (head dataProvider1) @?=
              ""
            ,testCase "list of length 1" $
              foldMapToString' ", "
                                f1'
                                (dataProvider1 !! 1) @?=
              "3"
            ,testCase "list of length >1" $
              foldMapToString' ", "
                                f1'
                                (dataProvider1 !! 2) @?=
              "3, 5, 7, 9"
            ,testCase "empty set" $
              foldMapToString' "\n"
                                f2'
                                (head dataProvider2) @?=
              ""
            ,testCase "set of size 1" $
              foldMapToString' "\n"
                                f2'
                                (dataProvider2 !! 1) @?=
              "-3-"
            ,testCase "set of size >1" $
              fromList (lines (foldMapToString' "\n"
                                                f2'
                                                (dataProvider2 !! 2))) @?=
              fromList ["-3-", "-7-", "-5-", "-9-"]]
