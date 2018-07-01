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
-- Test file for the Veca module.
-----------------------------------------------------------------------------

module RoverTests (roverTests)
where

import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC
import           Data.Map                        as M (Map, fromList)
import           Data.Monoid                     ((<>))
import qualified Data.Set                        as S (fromList)
import           Examples.Rover.Model
import           Models.Events
import           Models.LabelledTransitionSystem as LTS (LabelledTransitionSystem (..),
                                                         Path (..), paths')
import           Models.Name                     (Name (..))
import           Models.Named                    (Named (..))
import           Models.TimedAutomaton           as TA (Bounds (..),
                                                        ClockConstraint (..),
                                                        ClockOperator (..),
                                                        ClockReset (..),
                                                        Edge (..),
                                                        Expression (..),
                                                        Location (..),
                                                        TimedAutomaton (..),
                                                        VariableAssignment (..),
                                                        VariableType (..),
                                                        VariableTyping (..),
                                                        relabel)
import           Veca.Model
import           Veca.Operations

listDone :: Int -> Map (Name String) VariableTyping
listDone n = M.fromList
  [ ( Name ["done"]
    , VariableTyping (Name ["done"]) (IntType bounds) (Just $ Expression "0")
    )
  ]
  where bounds = Bounds 0 n

setDone :: Int -> VariableAssignment
setDone n = VariableAssignment (Name ["done"]) (Expression (show n))

roverTests :: TestTree
roverTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests = testGroup
  "Unit tests for the Rover Case Study"
  [uController, uStoreUnit, uPictureUnit, uVideoUnit, uAcquisitionUnit, uRover]

uController :: TestTree
uController = testGroup
  "Unit tests for Controller"
  [ testCase "basic component definition is valid"
  $   isValidComponent (componentType controllerUnit)
  @?= True
  , testCase "TA generation" $ cToTA controllerUnit @?= controllerTA
  ]

uStoreUnit :: TestTree
uStoreUnit = testGroup
  "Unit tests for Store Unit"
  [ testCase "basic component definition is valid"
  $   isValidComponent (componentType storeUnit)
  @?= True
  , testCase "TA generation" $ cToTA storeUnit @?= storeUnitTA
  ]

uPictureUnit :: TestTree
uPictureUnit = testGroup
  "Unit tests for Picture Unit"
  [ testCase "basic component definition is valid"
  $   isValidComponent (componentType pictureUnit)
  @?= True
  , testCase "TA generation" $ cToTA pictureUnit @?= pictureUnitTA
  ]

uVideoUnit :: TestTree
uVideoUnit = testGroup
  "Unit tests for Video Unit"
  [ testCase "basic component definition is valid"
  $   isValidComponent (componentType videoUnit)
  @?= True
  , testCase "paths" $ S.fromList computedVUPaths @?= S.fromList expectedVUPaths
  , testCase "isCPaths k1" $ (isCPath vuk1 <$> expectedVUPaths) @?= resk1
  , testCase "isCPaths k2" $ (isCPath vuk2 <$> expectedVUPaths) @?= resk2
  , testCase "isCPaths k3" $ (isCPath vuk3 <$> expectedVUPaths) @?= resk3
  , testCase "TA generation" $ cToTA videoUnit @?= videoUnitTA
  ]
  where computedVUPaths = paths' (behavior (componentType videoUnit))

uAcquisitionUnit :: TestTree
uAcquisitionUnit = testGroup
  "Unit tests for Acquisition Unit"
  [ testCase "composite component definition is valid"
    $   isValidComponent (componentType acquisitionUnit)
    @?= True
  ]

uRover :: TestTree
uRover = testGroup
  "Unit tests for Rover"
  [ testCase "composite component definition is valid"
  $   isValidComponent (componentType rover)
  @?= True
  , testCase "TA generation for the Rover (standalone)"
  $   (cTreeToTAList . cToCTree) rover
  @?= roverTAs
  ]

--
-- Results
--

expectedVUPaths :: [VPath]
expectedVUPaths =
  Path
    <$> [ []
        , [vut1]
        , [vut2]
        , [vut3]
        , [vut4]
        , [vut5]
        , [vut6]
        , [vut7]
        , [vut1, vut2]
        , [vut2, vut3]
        , [vut3, vut4]
        , [vut3, vut5]
        , [vut4, vut6]
        , [vut5, vut7]
        , [vut6, vut7]
        , [vut1, vut2, vut3]
        , [vut2, vut3, vut4]
        , [vut2, vut3, vut5]
        , [vut3, vut4, vut6]
        , [vut3, vut5, vut7]
        , [vut4, vut6, vut7]
        , [vut1, vut2, vut3, vut4]
        , [vut1, vut2, vut3, vut5]
        , [vut2, vut3, vut4, vut6]
        , [vut2, vut3, vut5, vut7]
        , [vut3, vut4, vut6, vut7]
        , [vut1, vut2, vut3, vut4, vut6]
        , [vut1, vut2, vut3, vut5, vut7]
        , [vut2, vut3, vut4, vut6, vut7]
        , [vut1, vut2, vut3, vut4, vut6, vut7]
        ]

resk1 :: [Bool]
resk1 =
  [ False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , True
  , False
  , True
  ]

resk2 :: [Bool]
resk2 =
  [ False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , True
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  ]

resk3 :: [Bool]
resk3 =
  [ False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , True
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  , False
  ]

--
-- test results
--

videoUnitTA :: VTA
videoUnitTA = TimedAutomaton
  v
  (Location <$> ["0", "1", "2", "3", "4", "5", "6"])
  (Location "0")
  []
  []
  clocksVU
  (listDone 6)
  (alphabet . behavior . componentType $ videoUnit)
  [ Edge (Location "0") (receive askVid) [] [ClockReset c1] [setDone 1] (Location "1")
  , Edge (Location "1") (invoke getVid)  [] [ClockReset c3] [setDone 3] (Location "2")
  , Edge (Location "2")
         (result getVid)
         [ClockConstraint c3 GE 0]
         [ClockReset c2]
         [setDone 4]
         (Location "3")
  , Edge (Location "3") tau [] [] [setDone 6] (Location "4")
  , Edge (Location "3") tau [] [] [setDone 6] (Location "5")
  , Edge (Location "4")
         (invoke storeVid)
         [ClockConstraint c2 GE 0]
         []
         [setDone 5]
         (Location "5")
  , Edge (Location "5")
         (reply askVid)
         [ClockConstraint c1 GE 44]
         []
         [setDone 2]
         (Location "6")
  , Edge (Location "6") tau [] [] [setDone 6] (Location "6")
  ]
  [ (Location "1", [ClockConstraint c1 LE 46])
  , (Location "2", [ClockConstraint c1 LE 46, ClockConstraint c3 LE 6])
  , (Location "3", [ClockConstraint c1 LE 46, ClockConstraint c2 LE 12])
  , (Location "4", [ClockConstraint c1 LE 46, ClockConstraint c2 LE 12])
  , (Location "5", [ClockConstraint c1 LE 46])
  ]
 where
  clocksVU = genClock <$> timeconstraints (componentType videoUnit)
  c1       = head clocksVU
  c2       = clocksVU !! 1
  c3       = clocksVU !! 2

pictureUnitTA :: VTA
pictureUnitTA = TimedAutomaton
  p
  (Location <$> ["0", "1", "2", "3", "4", "5", "6"])
  (Location "0")
  []
  []
  clocksPU
  (listDone 6)
  (alphabet . behavior . componentType $ pictureUnit)
  [ Edge (Location "0") (receive askPic) [] [ClockReset c1] [setDone 1] (Location "1")
  , Edge (Location "1") (invoke getPic)  [] [ClockReset c3] [setDone 3] (Location "2")
  , Edge (Location "2")
         (result getPic)
         [ClockConstraint c3 GE 0]
         [ClockReset c2]
         [setDone 4]
         (Location "3")
  , Edge (Location "3") tau [] [] [setDone 6] (Location "4")
  , Edge (Location "3") tau [] [] [setDone 6] (Location "5")
  , Edge (Location "4")
         (invoke storePic)
         [ClockConstraint c2 GE 0]
         []
         [setDone 5]
         (Location "5")
  , Edge (Location "5")
         (reply askPic)
         [ClockConstraint c1 GE 44]
         []
         [setDone 2]
         (Location "6")
  , Edge (Location "6") tau [] [] [setDone 6] (Location "6")
  ]
  [ (Location "1", [ClockConstraint c1 LE 46])
  , (Location "2", [ClockConstraint c1 LE 46, ClockConstraint c3 LE 6])
  , (Location "3", [ClockConstraint c1 LE 46, ClockConstraint c2 LE 12])
  , (Location "4", [ClockConstraint c1 LE 46, ClockConstraint c2 LE 12])
  , (Location "5", [ClockConstraint c1 LE 46])
  ]
 where
  clocksPU = genClock <$> timeconstraints (componentType pictureUnit)
  c1       = head clocksPU
  c2       = clocksPU !! 1
  c3       = clocksPU !! 2

storeUnitTA :: VTA
storeUnitTA = TimedAutomaton
  s
  (Location <$> ["0", "1"])
  (Location "0")
  []
  []
  []
  (listDone 3)
  (alphabet . behavior . componentType $ storeUnit)
  [ Edge (Location "0") (receive storePic) [] [] [setDone 2] (Location "1")
  , Edge (Location "0") (receive storeVid) [] [] [setDone 3] (Location "1")
  , Edge (Location "1") tau                [] [] [setDone 1] (Location "0")
  , Edge (Location "0") tau                [] [] [setDone 1] (Location "0")
  ]
  []

controllerTA :: VTA
controllerTA = TimedAutomaton
  c
  (Location <$> ["0", "1", "2", "3", "4", "5", "6"])
  (Location "0")
  []
  []
  clocksC
  (listDone 7)
  ((alphabet . behavior . componentType $ controllerUnit) ++ [tau])
  [ Edge (Location "0")
         (receive run)
         []
         [ClockReset c1]
         [setDone 1]
         (Location "1")
  , Edge (Location "1") (invoke askVid) [] [] [setDone 3] (Location "2")
  , Edge (Location "2") (result askVid) [] [] [setDone 4] (Location "3")
  , Edge (Location "3") (invoke askPic) [] [] [setDone 5] (Location "4")
  , Edge (Location "4") (result askPic) [] [] [setDone 6] (Location "5")
  , Edge (Location "5")
         (reply run)
         [ClockConstraint c1 GE 55]
         []
         [setDone 2]
         (Location "6")
  , Edge (Location "6") tau [] [] [setDone 7] (Location "6")
  ]
  [ (Location "1", [ClockConstraint c1 LE 60])
  , (Location "2", [ClockConstraint c1 LE 60])
  , (Location "3", [ClockConstraint c1 LE 60])
  , (Location "4", [ClockConstraint c1 LE 60])
  , (Location "5", [ClockConstraint c1 LE 60])
  ]
 where
  clocksC = genClock <$> timeconstraints (componentType controllerUnit)
  c1      = head clocksC

roverTAs :: [VTA]
roverTAs =
  [ prefixBy r $ relabel sub1 controllerTA
  , prefixBy (r <> a) $ relabel sub2 pictureUnitTA
  , prefixBy (r <> a) $ relabel sub3 videoUnitTA
  , prefixBy r $ relabel sub4 storeUnitTA
  ]
 where
  sub1 =
    lift [mksub (r <> n5) run, mksub (r <> n1) askPic, mksub (r <> n2) askVid]
  sub2 = lift
    [mksub (r <> n1) askPic, mksub (r <> n6) getPic, mksub (r <> n3) storePic]
  sub3 = lift
    [mksub (r <> n2) askVid, mksub (r <> n7) getVid, mksub (r <> n4) storeVid]
  sub4 = lift [mksub (r <> n3) storePic, mksub (r <> n4) storeVid]
  lift = foldMap (fLift [CReceive, CReply, CInvoke, CResult])
  mksub i o = (o, indexBy i o)
