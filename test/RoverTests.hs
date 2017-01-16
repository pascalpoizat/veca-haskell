-----------------------------------------------------------------------------
-- |
-- Module      :  TreeTests
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Test file for the Tree module.
-----------------------------------------------------------------------------

module RoverTests (roverTests)
where

import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC

import           Data.Map                 as M (fromList)
import           Data.Maybe               as X (fromJust, isJust, isNothing)
import           Data.Set                 as S (fromList)
import           LabelledTransitionSystem as L
import           Veca                     as IUT

roverTests :: TestTree
roverTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests for the Rover Case Study"
            [u_signature
            ,u_behavior
            ]

u_signature :: TestTree
u_signature =
  testGroup "Unit tests for Signature"
            [(testCase "signature is valid" $
              isJust rcsVideoUnitSignature @?= True)]

u_behavior :: TestTree
u_behavior =
  testGroup "Unit tests for Behavior"
            [(testCase "behavior is valid" $
              isJust rcsVideoUnitBehavior @?= True)]

-- the Rover Case Study
rcsVideoUnitSignature :: Maybe Signature
rcsVideoUnitSignature =
  signature (S.fromList ["askVid"])
            (S.fromList ["getVid","storeVid"])
            (M.fromList
               [("askVid","m1:{}")
               ,("getVid","m1:{}")
               ,("storeVid","m3:{url:String,file:File}")])
            (M.fromList
               [("askVid",Just "m4:{url:String}")
               ,("getVid",Just "m2:{data:RawVideo}")
               ,("storeVid",Nothing)])

rcsVideoUnitBehavior :: Maybe Behavior
rcsVideoUnitBehavior
  | isNothing rcsVideoUnitSignature = Nothing
  | otherwise =
    behavior (fromJust rcsVideoUnitSignature)
             (S.fromList
                [CTau
                ,CReceive "askVid"
                ,CReply "askVid"
                ,CInvoke "getVid"
                ,CResult "getVid"
                ,CInvoke "storeVid"])
             (S.fromList ["s0","s1","s2","s3","s4","s5","s6"])
             "s0"
             (S.fromList ["s6"])
             (S.fromList
                [Transition "s0"
                            (CReceive "askVid")
                            "s1"
                ,Transition "s1"
                            (CInvoke "getVid")
                            "s2"
                ,Transition "s2"
                            (CResult "getVid")
                            "s3"
                ,"s3" `ctau` "s4"
                ,"s3" `ctau` "s5"
                ,Transition "s4"
                            (CInvoke "storeVid")
                            "s5"
                ,Transition "s5"
                            (CReply "askVid")
                            "s6"])
