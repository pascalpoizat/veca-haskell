-----------------------------------------------------------------------------
-- |
-- Module      :  TimedAutomatonTests
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Test file for the TimedAutomaton module.
-----------------------------------------------------------------------------

module TimedAutomatonTests (timedAutomatonTests)
where

import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC

import           Data.Map              (empty)
import           Models.Events         (CIOEvent (..))
import           Models.TimedAutomaton

timedAutomatonTests :: TestTree
timedAutomatonTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests"
            [uAsXta]

uAsXta :: TestTree
uAsXta =
  testGroup "Unit tests for toXta"
            [(testCase "internal actions" $ (asXta ta_model001) @?= res1)
            ,(testCase "internal actions + single reset + single clause guard" $
              (asXta ta_model002) @?= res2)
            ,(testCase "internal actions + multiple resets + multiple clause guard" $
              (asXta ta_model003) @?= res3)
            ,(testCase "internal actions + both guard and reset on an edge" $
              (asXta ta_model004) @?= res4)
            ,(testCase "channels and synchronized actions" $
              (asXta ta_model005) @?= res5)
            ]
  where
        --
        ls = Location <$> [0..4::Int]
        cs = Clock <$> ["1","2"]
        tau = CTau
        invokeA = CInvoke "a"
        receiveA = CReceive "a"
        replyA = CReply "a"
        resultA = CResult "a"
        invokeB = CInvoke "b"
        receiveB = CReceive "b"
        replyB = CReply "b"
        resultB = CResult "b"
        --
        ta_model001 :: TimedAutomaton (CIOEvent String) Int
        ta_model001 =
          TimedAutomaton "Model001"
                         [ls!!0,ls!!1,ls!!2]
                         (ls!!0)
                         []
                         [tau]
                         [Edge (ls!!0) tau [] [] (ls!!1)
                         ,Edge (ls!!0) tau [] [] (ls!!2)]
                         empty
        res1 =
          unlines ["process Model001(){"
                  ,"state l_0, l_1, l_2;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { },"
                  ,"    l_0 -> l_2 { };"
                  ,"}"
                  ,"Process = Model001();"
                  ,"system Process;"]
        --
        ta_model002 :: TimedAutomaton (CIOEvent String) Int
        ta_model002 =
          TimedAutomaton
            "Model002"
            [ls!!0,ls!!1,ls!!2]
            (ls!!0)
            [cs!!0]
            [tau]
            [Edge (ls!!0) tau [] (ClockReset <$> [cs!!0]) (ls!!1)
            ,Edge (ls!!1) tau [ClockConstraint (cs!!0) GE 5] [] (ls!!2)]
            empty
        res2 =
          unlines ["process Model002(){"
                  ,"clock c_1;"
                  ,"state l_0, l_1, l_2;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { assign c_1 = 0; },"
                  ,"    l_1 -> l_2 { guard c_1 >= 5; };"
                  ,"}"
                  ,"Process = Model002();"
                  ,"system Process;"]
        --
        ta_model003 :: TimedAutomaton (CIOEvent String) Int
        ta_model003 =
          TimedAutomaton
            "Model003"
            [ls!!0,ls!!1,ls!!2]
            (ls!!0)
            [cs!!0,cs!!1]
            [tau]
            [Edge (ls!!0) tau [] (ClockReset <$> [cs!!0,cs!!1]) (ls!!1)
            ,Edge (ls!!1) tau [ClockConstraint (cs!!0) GE 5,ClockConstraint (cs!!1) GE 3] [] (ls!!2)]
            empty
        res3 =
          unlines ["process Model003(){"
                  ,"clock c_1, c_2;"
                  ,"state l_0, l_1, l_2;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { assign c_1 = 0, c_2 = 0; },"
                  ,"    l_1 -> l_2 { guard c_1 >= 5 && c_2 >= 3; };"
                  ,"}"
                  ,"Process = Model003();"
                  ,"system Process;"]
        --
        ta_model004 :: TimedAutomaton (CIOEvent String) Int
        ta_model004 =
          TimedAutomaton
            "Model004"
            [ls!!0,ls!!1,ls!!2,ls!!3]
            (ls!!0)
            [cs!!0]
            [tau]
            [Edge (ls!!0) tau [] (ClockReset <$> [cs!!0]) (ls!!1)
            ,Edge (ls!!1) tau [ClockConstraint (cs!!0) GE 5] (ClockReset <$> [cs!!0]) (ls!!2)
            ,Edge (ls!!2) tau [ClockConstraint (cs!!0) GE 3] [] (ls!!3)]
            empty
        res4 =
          unlines ["process Model004(){"
                  ,"clock c_1;"
                  ,"state l_0, l_1, l_2, l_3;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { assign c_1 = 0; },"
                  ,"    l_1 -> l_2 { guard c_1 >= 5; assign c_1 = 0; },"
                  ,"    l_2 -> l_3 { guard c_1 >= 3; };"
                  ,"}"
                  ,"Process = Model004();"
                  ,"system Process;"]
        --
        ta_model005 :: TimedAutomaton (CIOEvent String) Int
        ta_model005 =
          TimedAutomaton
            "Model005"
            [ls!!0,ls!!1,ls!!2,ls!!3,ls!!4]
            (ls!!0)
            []
            [receiveA,invokeB,resultB,replyA]
            [Edge (ls!!0) receiveA [] [] (ls!!1)
            ,Edge (ls!!1) invokeB [] [] (ls!!2)
            ,Edge (ls!!2) resultB [] [] (ls!!3)
            ,Edge (ls!!3) replyA [] [] (ls!!4)]
            empty
        res5 =
          unlines ["chan a, b;"
                  ,"process Model005(){"
                  ,"state l_0, l_1, l_2, l_3, l_4;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { sync a?; },"
                  ,"    l_1 -> l_2 { sync b!; },"
                  ,"    l_2 -> l_3 { sync b?; },"
                  ,"    l_3 -> l_4 { sync a!; };"
                  ,"}"
                  ,"Process = Model005();"
                  ,"system Process;"]
