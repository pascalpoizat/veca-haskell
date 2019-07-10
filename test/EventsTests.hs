-----------------------------------------------------------------------------
-- |
-- Module      :  EventsTests
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Test file for the Events module.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module EventsTests (eventsTests)
where

import           Test.Tasty
-- import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC

import           Models.Complementary
import           Models.TCommunication
import           Models.Events
import           Models.Internal

eventsTests :: TestTree
eventsTests = testGroup "Tests" [propertytestsTIOEvent, propertytestsCTIOEvent]

instance Arbitrary (TIOEvent Int) where
  arbitrary =
    oneof [return TTau
          ,return (TReceive 0)
          ,return (TSend 0)
          ,return (TReceive 1)
          ,return (TSend 1)]

propertytestsTIOEvent :: TestTree
propertytestsTIOEvent = testGroup
  "typeclass instance checking for TIOEvent"
  [ QC.testProperty "complementary . complementary == id" prop_1
  , QC.testProperty
    "forall x, isInput x implies not (isOutput x) and not (isInternal x)"
    prop_2
  , QC.testProperty
    "forall x, isOutput x implies not (isInput x) and not (isInternal x)"
    prop_3
  , QC.testProperty
    "forall x, isInternal x implies not (isOutput x) and not (isInput x)"
    prop_4
  , QC.testProperty
    "forall x, either isInternal x, isInput x, or isOutput x is true"
    prop_5
  , QC.testProperty "forall x, isInput x implies isOutput $ complementary x"
                    prop_6
  , QC.testProperty "forall x, isOutput x implies isInput $ complementary x"
                    prop_7
  , QC.testProperty
    "forall x, isInternal x implies isInternal $ complementary x"
    prop_8
  ]
 where
  prop_1 :: TIOEvent Int -> Bool
  prop_1 e = (complementary . complementary) e == e
  prop_2 :: TIOEvent Int -> Property
  prop_2 e = isInput e ==> ((not . isOutput) e && (not . isInternal) e)
  prop_3 :: TIOEvent Int -> Property
  prop_3 e = isOutput e ==> ((not . isInput) e && (not . isInternal) e)
  prop_4 :: TIOEvent Int -> Property
  prop_4 e = isInternal e ==> ((not . isOutput) e && (not . isInput) e)
  prop_5 :: TIOEvent Int -> Bool
  prop_5 e = isInternal e || isInput e || isOutput e
  prop_6 :: TIOEvent Int -> Property
  prop_6 e = isInput e ==> isOutput (complementary e)
  prop_7 :: TIOEvent Int -> Property
  prop_7 e = isOutput e ==> isInput (complementary e)
  prop_8 :: TIOEvent Int -> Property
  prop_8 e = isInternal e ==> isInternal (complementary e)

instance Arbitrary (CTIOEvent Int) where
  arbitrary =
    oneof [return CTTau
          ,return (CTInvoke 0)
          ,return (CTReceive 0)
          ,return (CTReply 0)
          ,return (CTResult 0)
          ,return (CTInvoke 1)
          ,return (CTReceive 1)
          ,return (CTReply 1)
          ,return (CTResult 1)]

propertytestsCTIOEvent :: TestTree
propertytestsCTIOEvent = testGroup
  "typeclass instance checking for CTIOEvent"
  [ QC.testProperty "complementary . complementary == id" prop_1
  , QC.testProperty
    "forall x, isInput x implies not (isOutput x) and not (isInternal x)"
    prop_2
  , QC.testProperty
    "forall x, isOutput x implies not (isInput x) and not (isInternal x)"
    prop_3
  , QC.testProperty
    "forall x, isInternal x implies not (isOutput x) and not (isInput x)"
    prop_4
  , QC.testProperty
    "forall x, either isInternal x, isInput x, or isOutput x is true"
    prop_5
  , QC.testProperty "forall x, isInput x implies isOutput $ complementary x"
                    prop_6
  , QC.testProperty "forall x, isOutput x implies isInput $ complementary x"
                    prop_7
  , QC.testProperty
    "forall x, isInternal x implies isInternal $ complementary x"
    prop_8
  ]
 where
  prop_1 :: CTIOEvent Int -> Bool
  prop_1 e = (complementary . complementary) e == e
  prop_2 :: CTIOEvent Int -> Property
  prop_2 e = isInput e ==> ((not . isOutput) e && (not . isInternal) e)
  prop_3 :: CTIOEvent Int -> Property
  prop_3 e = isOutput e ==> ((not . isInput) e && (not . isInternal) e)
  prop_4 :: CTIOEvent Int -> Property
  prop_4 e = isInternal e ==> ((not . isOutput) e && (not . isInput) e)
  prop_5 :: CTIOEvent Int -> Bool
  prop_5 e = isInternal e || isInput e || isOutput e
  prop_6 :: CTIOEvent Int -> Property
  prop_6 e = isInput e ==> isOutput (complementary e)
  prop_7 :: CTIOEvent Int -> Property
  prop_7 e = isOutput e ==> isInput (complementary e)
  prop_8 :: CTIOEvent Int -> Property
  prop_8 e = isInternal e ==> isInternal (complementary e)

