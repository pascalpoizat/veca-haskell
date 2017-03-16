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
import           Models.Communication
import           Models.Events
import           Models.Internal

eventsTests :: TestTree
eventsTests = testGroup "Tests" [propertytestsIOEvent,propertytestsCIOEvent]

instance Arbitrary (IOEvent Int) where
  arbitrary =
    oneof [return Tau
          ,return (Receive 0)
          ,return (Send 0)
          ,return (Receive 1)
          ,return (Send 1)]

propertytestsIOEvent :: TestTree
propertytestsIOEvent =
  testGroup "typeclass instance checking for IOEvent"
            [QC.testProperty "complementary . complementary == id" prop_1
            ,QC.testProperty "forall x, isInput x implies not (isOutput x) and not (isInternal x)" prop_2
            ,QC.testProperty "forall x, isOutput x implies not (isInput x) and not (isInternal x)" prop_3
            ,QC.testProperty "forall x, isInternal x implies not (isOutput x) and not (isInput x)" prop_4
            ,QC.testProperty "forall x, either isInternal x, isInput x, or isOutput x is true" prop_5
            ,QC.testProperty "forall x, isInput x implies isOutput $ complementary x" prop_6
            ,QC.testProperty "forall x, isOutput x implies isInput $ complementary x" prop_7
            ,QC.testProperty "forall x, isInternal x implies isInternal $ complementary x" prop_8
            ]
  where
    prop_1 :: (IOEvent Int) -> Bool
    prop_1 e = (complementary.complementary) e == e
    prop_2 :: (IOEvent Int) -> Property
    prop_2 e = (isInput e) ==> ((not . isOutput) e && (not . isInternal) e)
    prop_3 :: (IOEvent Int) -> Property
    prop_3 e = (isOutput e) ==> ((not . isInput) e && (not . isInternal) e)
    prop_4 :: (IOEvent Int) -> Property
    prop_4 e = (isInternal e) ==> ((not . isOutput) e && (not . isInput) e)
    prop_5 :: (IOEvent Int) -> Bool
    prop_5 e = (isInternal e) || (isInput e) || (isOutput e)
    prop_6 :: (IOEvent Int) -> Property
    prop_6 e = (isInput e) ==> (isOutput $ complementary e)
    prop_7 :: (IOEvent Int) -> Property
    prop_7 e = (isOutput e) ==> (isInput $ complementary e)
    prop_8 :: (IOEvent Int) -> Property
    prop_8 e = (isInternal e) ==> (isInternal $ complementary e)

instance Arbitrary (CIOEvent Int) where
  arbitrary =
    oneof [return CTau
          ,return (CInvoke 0)
          ,return (CReceive 0)
          ,return (CReply 0)
          ,return (CResult 0)
          ,return (CInvoke 1)
          ,return (CReceive 1)
          ,return (CReply 1)
          ,return (CResult 1)]

propertytestsCIOEvent :: TestTree
propertytestsCIOEvent =
  testGroup "typeclass instance checking for CIOEvent"
            [QC.testProperty "complementary . complementary == id" prop_1
            ,QC.testProperty "forall x, isInput x implies not (isOutput x) and not (isInternal x)" prop_2
            ,QC.testProperty "forall x, isOutput x implies not (isInput x) and not (isInternal x)" prop_3
            ,QC.testProperty "forall x, isInternal x implies not (isOutput x) and not (isInput x)" prop_4
            ,QC.testProperty "forall x, either isInternal x, isInput x, or isOutput x is true" prop_5
            ,QC.testProperty "forall x, isInput x implies isOutput $ complementary x" prop_6
            ,QC.testProperty "forall x, isOutput x implies isInput $ complementary x" prop_7
            ,QC.testProperty "forall x, isInternal x implies isInternal $ complementary x" prop_8
            ]
  where
    prop_1 :: (CIOEvent Int) -> Bool
    prop_1 e = (complementary.complementary) e == e
    prop_2 :: (CIOEvent Int) -> Property
    prop_2 e = (isInput e) ==> ((not . isOutput) e && (not . isInternal) e)
    prop_3 :: (CIOEvent Int) -> Property
    prop_3 e = (isOutput e) ==> ((not . isInput) e && (not . isInternal) e)
    prop_4 :: (CIOEvent Int) -> Property
    prop_4 e = (isInternal e) ==> ((not . isOutput) e && (not . isInput) e)
    prop_5 :: (CIOEvent Int) -> Bool
    prop_5 e = (isInternal e) || (isInput e) || (isOutput e)
    prop_6 :: (CIOEvent Int) -> Property
    prop_6 e = (isInput e) ==> (isOutput $ complementary e)
    prop_7 :: (CIOEvent Int) -> Property
    prop_7 e = (isOutput e) ==> (isInput $ complementary e)
    prop_8 :: (CIOEvent Int) -> Property
    prop_8 e = (isInternal e) ==> (isInternal $ complementary e)

