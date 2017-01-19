-----------------------------------------------------------------------------
-- |
-- Module      :  RoverTests
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
import           Data.Set                 as S (Set, fromList)
import           LabelledTransitionSystem as L
import           Veca                     as IUT

roverTests :: TestTree
roverTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests for the Rover Case Study"
            [u_videounit
            ]

u_videounit :: TestTree
u_videounit =
  testGroup "Unit tests for VideoUnit"
            [(testCase "basic component definition is valid" $
              isValidComponent rcsVideoUnitComponent @?= True)]

-- the Rover Case Study

rcsVideoUnitComponent :: Component
rcsVideoUnitComponent =
  BasicComponent rcsVideoUnitSignature rcsVideoUnitBehavior rcsVideoUnitTimeConstraints

rcsVideoUnitSignature :: Signature
rcsVideoUnitSignature =
  Signature (S.fromList $ map Operation ["askVid"])
            (S.fromList $ map Operation ["getVid","storeVid"])
            (M.fromList
               [(Operation "askVid",Message "m1:{}")
               ,(Operation "getVid",Message "m1:{}")
               ,(Operation "storeVid",Message "m3:{url:String,file:File}")])
            (M.fromList
               [(Operation "askVid",Just . Message $ "m4:{url:String}")
               ,(Operation "getVid",Just . Message $ "m2:{data:RawVideo}")
               ,(Operation "storeVid",Nothing)])

rcsVideoUnitBehavior :: Behavior
rcsVideoUnitBehavior =
  LTS (S.fromList
         [CTau
         ,CReceive . Operation $ "askVid"
         ,CReply . Operation $ "askVid"
         ,CInvoke . Operation $ "getVid"
         ,CResult . Operation $ "getVid"
         ,CInvoke . Operation $ "storeVid"])
      (S.fromList $ map State ["s0","s1","s2","s3","s4","s5","s6"])
      (State "s0")
      (S.fromList $ map State ["s6"])
      (S.fromList
         [Transition (State "s0")
                     (CReceive . Operation $ "askVid")
                     (State "s1")
         ,Transition (State "s1")
                     (CInvoke . Operation $ "getVid")
                     (State "s2")
         ,Transition (State "s2")
                     (CResult . Operation $ "getVid")
                     (State "s3")
         ,State "s3" `ctau` State "s4"
         ,State "s3" `ctau` State "s5"
         ,Transition (State "s4")
                     (CInvoke . Operation $ "storeVid")
                     (State "s5")
         ,Transition (State "s5")
                     (CReply . Operation $ "askVid")
                     (State "s6")])

rcsVideoUnitTimeConstraints :: S.Set TimeConstraint
rcsVideoUnitTimeConstraints =
  S.fromList
    [TimeConstraint (CReceive . Operation $ "askVid")
                    (CReply . Operation $ "askVid")
                    44
                    46
    ,TimeConstraint (CResult . Operation $ "getVid")
                    (CInvoke . Operation $ "storeVid")
                    0
                    12
    ,TimeConstraint (CInvoke . Operation $ "getVid")
                    (CResult . Operation $ "getVid")
                    0
                    6]
