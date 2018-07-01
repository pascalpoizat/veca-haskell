-----------------------------------------------------------------------------
-- |
-- Module      :  SubstitutionTests
-- Copyright   :  (c) 2018 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Test file for the Substitution module.
-----------------------------------------------------------------------------
module SubstitutionTests (substitutionTests)
where

import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC

import           Transformations.Substitution

substitutionTests :: TestTree
substitutionTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests = testGroup
  "Unit tests for the Substitution module"
  [uApply, uBoundvariables, uFreevariables, uIsBound, uIsFree]

sub1 :: Substitution String
sub1 = []

sub3 :: Substitution String
sub3 = [("a", "b"), ("c", "d"), ("e", "f")]

sub4 :: Substitution String
sub4 = [("a", "b"), ("c", "d"), ("a", "c")]

uApply :: TestTree
uApply = testGroup
  "Unit tests for apply"
  [ testCase "empty substitution" $ apply sub1 "a" @?= "a"
  , testCase "free variable" $ apply sub3 "f" @?= "f"
  , testCase "bound variable (no duplicate key)" $ apply sub3 "c" @?= "d"
  , testCase "bound variable (duplicate key)" $ apply sub4 "a" @?= "c"
  ]

uBoundvariables :: TestTree
uBoundvariables = testGroup
  "Unit tests for boundvariables"
  [ testCase "empty substitution" $ boundvariables sub1 @?= []
  , testCase "simple substitution" $ boundvariables sub3 @?= ["a", "c", "e"]
  , testCase "substitution with duplicate key"
  $   boundvariables sub4
  @?= ["a", "c"]
  ]

uFreevariables :: TestTree
uFreevariables = testGroup
  "Unit tests for freevariables"
  [ testCase "empty substitution (empty)" $ freevariables [] sub1 @?= []
  , testCase "empty substitution (order 1)"
  $   freevariables sub1 ["c", "a"]
  @?= ["c", "a"]
  , testCase "empty substitution (order 2)"
  $   freevariables sub1 ["a", "c"]
  @?= ["a", "c"]
  , testCase "simple substitution (empty)" $ freevariables sub3 [] @?= []
  , testCase "simple substitution (order 1)"
  $   freevariables sub3 ["f", "c", "g", "a"]
  @?= ["f", "g"]
  , testCase "simple substitution (order 2)"
  $   freevariables sub3 ["a", "g", "c", "f"]
  @?= ["g", "f"]
  , testCase "substitution with duplicate key (empty)"
  $   freevariables sub4 []
  @?= []
  , testCase "substitution with duplicate key (order 1)"
  $   freevariables sub4 ["f", "c", "g", "a"]
  @?= ["f", "g"]
  , testCase "substitution with duplicate key (order 2)"
  $   freevariables sub4 ["a", "g", "c", "f"]
  @?= ["g", "f"]
  ]

uIsBound :: TestTree
uIsBound = testGroup
  "Unit tests for isBound" 
  [ testCase "empty substitution" $ isBound sub1 "x" @?= False
  , testCase "free in the substitution" $ isBound sub3 "x" @?= False
  , testCase "bound in the substitution" $ isBound sub3 "c" @?= True
  ]

uIsFree :: TestTree
uIsFree = testGroup
  "Unit tests for isFree"
  [ testCase "empty substitution" $ isFree sub1 "x" @?= True
  , testCase "free in the substitution" $ isFree sub3 "x" @?= True
  , testCase "bound in the substitution" $ isFree sub3 "c" @?= False
  ]

