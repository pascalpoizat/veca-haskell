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

import           Data.Map              (empty, fromList)
import           Models.Events         (CIOEvent (..))
import           Models.Name           (Name (..))
import           Models.TimedAutomaton as TA

timedAutomatonTests :: TestTree
timedAutomatonTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests"
            [uEq
            ,uLocationKind
            ,uAsXta]

c :: Clock
c = Clock "c"
c' :: Clock
c' = Clock "c'"
l :: Location String
l = Location "l"
l' :: Location String
l' = Location "l'"
cc1 :: ClockConstraint
cc1 = ClockConstraint c TA.LT 5
cc1a :: ClockConstraint
cc1a = ClockConstraint c' TA.LT 5
cc1b :: ClockConstraint
cc1b = ClockConstraint c TA.GT 5
cc1c :: ClockConstraint
cc1c = ClockConstraint c TA.LT 6
e :: Edge String String
e = Edge l "a" [] [] [] l'
e2 :: Edge String String
e2 = Edge l "a" [cc1, cc1a] [ClockReset c, ClockReset c'] [] l'
e2x :: Edge String String
e2x = Edge l "a" [cc1a, cc1] [ClockReset c, ClockReset c'] [] l'
e2y :: Edge String String
e2y = Edge l "a" [cc1, cc1a] [ClockReset c', ClockReset c] [] l'
e2a :: Edge String String
e2a = Edge l' "a" [cc1, cc1a] [ClockReset c, ClockReset c'] [] l'
e2b :: Edge String String
e2b = Edge l "a" [cc1, cc1a] [ClockReset c, ClockReset c'] [] l
e2c :: Edge String String
e2c = Edge l "b" [cc1, cc1a] [ClockReset c, ClockReset c'] [] l'
e2d :: Edge String String
e2d = Edge l "a" [cc1, cc1b] [ClockReset c, ClockReset c'] [] l'
e2e :: Edge String String
e2e = Edge l "a" [cc1, cc1a] [ClockReset c'] [] l'

uEq :: TestTree
uEq =
  testGroup "Unit tests for equality"
            [testCase "equality on clocks" $
              (c == c) @?= True
            ,testCase "equality on clocks (not equal)" $
              (c == c') @?= False
            ,testCase "equality on locations" $
              (l == l) @?= True
            ,testCase "equality on locations (not equal)" $
              (l == l') @?= False
            ,testCase "equality on clock constraint" $
              (cc1 == cc1) @?= True
            ,testCase "equality on clock constraint (not equal / clock)" $
              (cc1 == cc1a) @?= False
            ,testCase "equality on clock constraint (not equal / operator)" $
              (cc1 == cc1b) @?= False
            ,testCase "equality on clock constraint (not equal / value)" $
              (cc1 == cc1c) @?= False
            ,testCase "equality on edge (no guard, no resets)" $
             (e == e) @?= True
            ,testCase "equality on edge (guard + resets)" $
             (e2 == e2) @?= True
            ,testCase "equality on edge (reordering of guard)" $
             (e2 == e2x) @?= True
            ,testCase "equality on edge (reordering of resets)" $
             (e2 == e2y) @?= True
            ,testCase "equality on edge (not equal / source)" $
             (e2 == e2a) @?= False
            ,testCase "equality on edge (not equal / target)" $
             (e2 == e2b) @?= False
            ,testCase "equality on edge (not equal / action)" $
             (e2 == e2c) @?= False
            ,testCase "equality on edge (not equal / guard)" $
             (e2 == e2d) @?= False
            ,testCase "equality on edge (not equal / resets)" $
             (e2 == e2e) @?= False
            ]

uLocationKind :: TestTree
uLocationKind =
  testGroup "Unit tests for location kinds"
    [testCase "isCommitted on a committed state" $
    isCommitted ta_model002 (head ls) @?= True
    ,testCase "isCommitted on an urgent state" $
    isCommitted ta_model002 (ls !! 1) @?= False
    ,testCase "isCommitted on an normal state" $
    isCommitted ta_model002 (ls !! 2) @?= False
    --
    ,testCase "isUrgent on a committed state" $
    isUrgent ta_model002 (head ls) @?= False
    ,testCase "isUrgent on an urgent state" $
    isUrgent ta_model002 (ls !! 1) @?= True
    ,testCase "isUrgent on an normal state" $
    isUrgent ta_model002 (ls !! 2) @?= False
    --
    ,testCase "setCommited on a committed state" $
    isCommitted (setCommitted ta_model002 (head ls)) (head ls) @?= True
    ,testCase "setCommited on an urgent state (1)" $
    isCommitted (setCommitted ta_model002 (ls !! 1)) (ls !! 1) @?= True
    ,testCase "setCommited on an urgent state (2)" $
    isUrgent (setCommitted ta_model002 (ls !! 1)) (ls !! 1) @?= False
    ,testCase "setCommited on a normal state" $
    isCommitted (setCommitted ta_model002 (ls !! 2)) (ls !! 2) @?= True
    --
    ,testCase "setUrgent on an urgent state" $
    isUrgent (setUrgent ta_model002 (ls !! 1)) (ls !! 1) @?= True
    ,testCase "setUrgent on a committed state (1)" $
    isUrgent (setUrgent ta_model002 (head ls)) (head ls) @?= True
    ,testCase "setUrgent on a commited state (2)" $
    isCommitted (setUrgent ta_model002 (head ls)) (head ls) @?= False
    ,testCase "setUrgent on a normal state" $
    isUrgent (setUrgent ta_model002 (ls !! 2)) (ls !! 2) @?= True
    ]
  where
    ls = Location <$> [0 .. 4 :: Int]
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
    ta_model002 :: TimedAutomaton (CIOEvent String) Int
    ta_model002 =
      TimedAutomaton
      (Name ["Model002"])
      [head ls,ls !! 1,ls !! 2]
      (head ls)
      [head ls]
      [ls !! 1]
      [head cs]
      empty
      [tau]
      [Edge (head ls)
            tau
            []
            (ClockReset <$> [head cs])
            []
            (ls !! 1)
      ,Edge (ls !! 1)
            tau
            [ClockConstraint (head cs)
                             GE
                             5]
            []
            []
            (ls !! 2)]
      []

uAsXta :: TestTree
uAsXta =
  testGroup "Unit tests for toXta"
            [testCase "internal actions" $
            asXta (TimedAutomataNetwork [ta_model001]) @?= res1
            ,testCase "internal actions + single reset + single clause guard" $
             asXta (TimedAutomataNetwork [ta_model002]) @?= res2
            ,testCase "internal actions + multiple resets + multiple clause guard" $
             asXta (TimedAutomataNetwork [ta_model003]) @?= res3
            ,testCase "internal actions + both guard and reset on an edge" $
             asXta (TimedAutomataNetwork [ta_model004]) @?= res4
            ,testCase "channels and synchronized actions" $
             asXta (TimedAutomataNetwork [ta_model005]) @?= res5
            ,testCase "invariants (single occurence of state)" $
             asXta (TimedAutomataNetwork [ta_model006]) @?= res6
            ,testCase "invariants (multiple occurence of state)" $
             asXta (TimedAutomataNetwork [ta_model006']) @?= res6]
  where
        --
        ls = Location <$> [0 .. 4 :: Int]
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
        ta_model001
          :: TimedAutomaton (CIOEvent String) Int
        ta_model001 =
          TimedAutomaton
            (Name ["Model001"])
            [head ls,ls !! 1,ls !! 2]
            (head ls)
            []
            []
            []
            empty
            [tau]
            [Edge (head ls)
                  tau
                  []
                  []
                  []
                  (ls !! 1)
            ,Edge (head ls)
                  tau
                  []
                  []
                  []
                  (ls !! 2)]
            []
        res1 =
          unlines [""
                  ,"process Model001(){"
                  ,"state l_0, l_1, l_2;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { },"
                  ,"    l_0 -> l_2 { };"
                  ,"}"
                  ,""
                  ,"Process_Model001 = Model001();"
                  ,"system Process_Model001;"]
        --
        ta_model002
          :: TimedAutomaton (CIOEvent String) Int
        ta_model002 =
          TimedAutomaton
            (Name ["Model002"])
            [head ls,ls !! 1,ls !! 2]
            (head ls)
            [head ls]
            [ls !! 1, ls !! 2]
            [head cs]
            empty
            [tau]
            [Edge (head ls)
                  tau
                  []
                  (ClockReset <$> [head cs])
                  []
                  (ls !! 1)
            ,Edge (ls !! 1)
                  tau
                  [ClockConstraint (head cs)
                                   GE
                                   5]
                  []
                  []
                  (ls !! 2)]
            []
        res2 =
          unlines [""
                  ,"process Model002(){"
                  ,"clock c_1;"
                  ,"state l_0, l_1, l_2;"
                  ,"commit l_0;"
                  ,"urgent l_1, l_2;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { assign c_1 = 0; },"
                  ,"    l_1 -> l_2 { guard c_1 >= 5; };"
                  ,"}"
                  ,""
                  ,"Process_Model002 = Model002();"
                  ,"system Process_Model002;"]
        --
        ta_model003
          :: TimedAutomaton (CIOEvent String) Int
        ta_model003 =
          TimedAutomaton
            (Name ["Model003"])
            [head ls,ls !! 1,ls !! 2]
            (head ls)
            [ls !! 1, ls !! 2]
            []
            [head cs,cs !! 1]
            (fromList [(Name ["x"], VariableTyping (Name ["x"]) (IntType NoBounds) Nothing)
                      ,(Name ["y"], VariableTyping (Name ["y"]) (IntType NoBounds) (Just $ Expression "0"))
                      ,(Name ["z", "1"], VariableTyping (Name ["z","1"]) (IntType (Bounds 0 4)) (Just $ Expression "1"))
                      ])
            [tau]
            [Edge (head ls)
                  tau
                  []
                  (ClockReset <$> [head cs,cs !! 1])
                  [VariableAssignment (Name ["x"]) (Expression "3")]
                  (ls !! 1)
            ,Edge (ls !! 1)
                  tau
                  [ClockConstraint (head cs)
                                   GE
                                   5
                  ,ClockConstraint (cs !! 1)
                                   GE
                                   3]
                  []
                  [VariableAssignment (Name ["y"]) (Expression "1")
                  ,VariableAssignment (Name ["x"]) (Expression "2")]
                  (ls !! 2)]
            []
        res3 =
          unlines [""
                  ,"process Model003(){"
                  ,"clock c_1, c_2;"
                  ,"int x;"
                  ,"int y = 0;"
                  ,"int[0,4] z_1 = 1;"
                  ,"state l_0, l_1, l_2;"
                  ,"commit l_1, l_2;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { assign x = 3, c_1 = 0, c_2 = 0; },"
                  ,"    l_1 -> l_2 { guard c_1 >= 5 && c_2 >= 3; assign y = 1, x = 2; };"
                  ,"}"
                  ,""
                  ,"Process_Model003 = Model003();"
                  ,"system Process_Model003;"]
        --
        ta_model004
          :: TimedAutomaton (CIOEvent String) Int
        ta_model004 =
          TimedAutomaton
            (Name ["Model004"])
            [head ls,ls !! 1,ls !! 2,ls !! 3]
            (head ls)
            []
            []
            [head cs]
            empty
            [tau]
            [Edge (head ls)
                  tau
                  []
                  (ClockReset <$> [head cs])
                  []
                  (ls !! 1)
            ,Edge (ls !! 1)
                  tau
                  [ClockConstraint (head cs)
                                   GE
                                   5]
                  (ClockReset <$> [head cs])
                  []
                  (ls !! 2)
            ,Edge (ls !! 2)
                  tau
                  [ClockConstraint (head cs)
                                   GE
                                   3]
                  []
                  []
                  (ls !! 3)]
            []
        res4 =
          unlines [""
                  ,"process Model004(){"
                  ,"clock c_1;"
                  ,"state l_0, l_1, l_2, l_3;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { assign c_1 = 0; },"
                  ,"    l_1 -> l_2 { guard c_1 >= 5; assign c_1 = 0; },"
                  ,"    l_2 -> l_3 { guard c_1 >= 3; };"
                  ,"}"
                  ,""
                  ,"Process_Model004 = Model004();"
                  ,"system Process_Model004;"]
        --
        ta_model005
          :: TimedAutomaton (CIOEvent String) Int
        ta_model005 =
          TimedAutomaton
            (Name ["Model005"])
            [head ls,ls !! 1,ls !! 2,ls !! 3,ls !! 4]
            (head ls)
            []
            []
            []
            empty
            [receiveA,invokeB,resultB,replyA]
            [Edge (head ls)
                  receiveA
                  []
                  []
                  []
                  (ls !! 1)
            ,Edge (ls !! 1)
                  invokeB
                  []
                  []
                  []
                  (ls !! 2)
            ,Edge (ls !! 2)
                  resultB
                  []
                  []
                  []
                  (ls !! 3)
            ,Edge (ls !! 3)
                  replyA
                  []
                  []
                  []
                  (ls !! 4)]
            []
        res5 =
          unlines ["chan a_req, b_req, b_res, a_res;"
                  ,"process Model005(){"
                  ,"state l_0, l_1, l_2, l_3, l_4;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { sync a_req?; },"
                  ,"    l_1 -> l_2 { sync b_req!; },"
                  ,"    l_2 -> l_3 { sync b_res?; },"
                  ,"    l_3 -> l_4 { sync a_res!; };"
                  ,"}"
                  ,""
                  ,"Process_Model005 = Model005();"
                  ,"system Process_Model005;"]
        --
        ta_model006
          :: TimedAutomaton (CIOEvent String) Int
        ta_model006 =
          TimedAutomaton
            (Name ["Model006"])
            [head ls,ls !! 1,ls !! 2]
            (head ls)
            []
            []
            [head cs,cs !! 1]
            empty
            [tau]
            [Edge (head ls)
                  tau
                  []
                  (ClockReset <$> [head cs,cs !! 1])
                  []
                  (ls !! 1)
            ,Edge (ls !! 1)
                  tau
                  [ClockConstraint (head cs)
                                   TA.GT
                                   2
                  ,ClockConstraint (cs !! 1)
                                   TA.GT
                                   4]
                  []
                  []
                  (ls !! 2)]
            [(ls !! 1
             ,[ClockConstraint (head cs)
                               TA.LT
                               10
              ,ClockConstraint (cs !! 1)
                               TA.LT
                               8])]
        --
        ta_model006'
          :: TimedAutomaton (CIOEvent String) Int
        ta_model006' =
          TimedAutomaton
            (Name ["Model006"])
            [head ls,ls !! 1,ls !! 2]
            (head ls)
            []
            []
            [head cs,cs !! 1]
            empty
            [tau]
            [Edge (head ls)
                  tau
                  []
                  (ClockReset <$> [head cs,cs !! 1])
                  []
                  (ls !! 1)
            ,Edge (ls !! 1)
                  tau
                  [ClockConstraint (head cs)
                                   TA.GT
                                   2
                  ,ClockConstraint (cs !! 1)
                                   TA.GT
                                   4]
                  []
                  []
                  (ls !! 2)]
            [(ls !! 1
             ,[ClockConstraint (head cs)
                               TA.LT
                               10])
            ,(ls !! 1
             ,[ClockConstraint (cs !! 1)
                               TA.LT
                               8])]
        res6 =
          unlines [""
                  ,"process Model006(){"
                  ,"clock c_1, c_2;"
                  ,"state l_0, l_1 { c_1 < 10 && c_2 < 8 }, l_2;"
                  ,"init l_0;"
                  ,"trans"
                  ,"    l_0 -> l_1 { assign c_1 = 0, c_2 = 0; },"
                  ,"    l_1 -> l_2 { guard c_1 > 2 && c_2 > 4; };"
                  ,"}"
                  ,""
                  ,"Process_Model006 = Model006();"
                  ,"system Process_Model006;"]
