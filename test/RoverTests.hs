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
import           Data.Map                        (fromList)
import qualified Data.Set                        as S (fromList)
import           Models.Events
import           Models.LabelledTransitionSystem
import           Models.Name                     (Name (..))
import           Models.TimedAutomaton
import           Veca.Veca

roverTests :: TestTree
roverTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests for the Rover Case Study"
            [uController
            ,uStoreUnit
            ,uPictureUnit
            ,uVideoUnit
            ,uAcquisitionUnit
            ,uRover
            ]

uController :: TestTree
uController =
  testGroup "Unit tests for Controller"
            [testCase "basic component definition is valid" $
              isValidComponent controllerUnit @?= True
            ,testCase "TA generation" $
              cToTA controllerUnit @?= controllerTA]

uStoreUnit :: TestTree
uStoreUnit =
  testGroup "Unit tests for Store Unit"
            [testCase "basic component definition is valid" $
              isValidComponent storeUnit @?= True
            ,testCase "TA generation" $
              cToTA storeUnit @?= storeUnitTA]

uPictureUnit :: TestTree
uPictureUnit =
  testGroup "Unit tests for Picture Unit"
            [testCase "basic component definition is valid" $
              isValidComponent pictureUnit @?= True
            ,testCase "TA generation" $
              cToTA pictureUnit @?= pictureUnitTA]

uVideoUnit :: TestTree
uVideoUnit =
  testGroup "Unit tests for Video Unit"
            [testCase "basic component definition is valid" $
             isValidComponent videoUnit @?= True
            ,testCase "paths" $
             S.fromList computedVUPaths @?= S.fromList expectedVUPaths
            ,testCase "isCPaths k1" $
             (isCPath vuk1 <$> expectedVUPaths) @?= resk1
            ,testCase "isCPaths k2" $
             (isCPath vuk2 <$> expectedVUPaths) @?= resk2
            ,testCase "isCPaths k3" $
             (isCPath vuk3 <$> expectedVUPaths) @?= resk3
            ,testCase "TA generation" $
             cToTA videoUnit @?= videoUnitTA]
  where
    computedVUPaths = paths' (behavior videoUnit)

uAcquisitionUnit :: TestTree
uAcquisitionUnit =
  testGroup "Unit tests for Acquisition Unit"
            [testCase "composite component definition is valid" $
              isValidComponent acquisitionUnit @?= True]

uRover :: TestTree
uRover =
  testGroup "Unit tests for Rover"
            [testCase "composite component definition is valid" $
              isValidComponent rover @?= True
            ,testCase "TA generation for the Rover (standalone)" $
             (flatten . cToTATree) rover @?= roverTAs]

--
-- Results
--

expectedVUPaths :: [VPath]
expectedVUPaths =
  Path <$>
  [[]
  ,[vut1],[vut2],[vut3],[vut4],[vut5],[vut6],[vut7]
  ,[vut1,vut2],[vut2,vut3],[vut3,vut4],[vut3,vut5],[vut4,vut6],[vut5,vut7],[vut6,vut7]
  ,[vut1,vut2,vut3],[vut2,vut3,vut4],[vut2,vut3,vut5],[vut3,vut4,vut6],[vut3,vut5,vut7],[vut4,vut6,vut7]
  ,[vut1,vut2,vut3,vut4],[vut1,vut2,vut3,vut5],[vut2,vut3,vut4,vut6],[vut2,vut3,vut5,vut7],[vut3,vut4,vut6,vut7]
  ,[vut1,vut2,vut3,vut4,vut6],[vut1,vut2,vut3,vut5,vut7],[vut2,vut3,vut4,vut6,vut7]
  ,[vut1,vut2,vut3,vut4,vut6,vut7]]

resk1 :: [Bool]
resk1 =
  [False
  ,False,False,False,False,False,False,False
  ,False,False,False,False,False,False,False
  ,False,False,False,False,False,False
  ,False,False,False,False,False
  ,False,True,False
  ,True]

resk2 :: [Bool]
resk2 =
  [False
  ,False,False,False,False,False,False,False
  ,False,False,False,False,False,False,False
  ,False,False,False,True,False,False
  ,False,False,False,False,False
  ,False,False,False
  ,False]

resk3 :: [Bool]
resk3 =
  [False
  ,False,False,False,False,False,False,False
  ,False,True,False,False,False,False,False
  ,False,False,False,False,False,False
  ,False,False,False,False,False
  ,False,False,False
  ,False]

--
-- The Rover Case Study
--

askVid :: Operation
askVid = mkOperation "askVid"
getVid :: Operation
getVid = mkOperation "getVid"
storeVid :: Operation
storeVid = mkOperation "storeVid"
askPic :: Operation
askPic = mkOperation "askPic"
getPic :: Operation
getPic = mkOperation "getPic"
storePic :: Operation
storePic = mkOperation "storePic"

m1 :: Message
m1 = mkMessage "m1" "{}"
m1s :: Message
m1s = mkMessage "m1s" "{url:String,file:File}"
m4 :: Message
m4 = mkMessage "m4" "{url:String}"

nameController :: Name
nameController = Name ["controller"]

nameStoreUnit :: Name
nameStoreUnit = Name ["storeUnit"]

namePictureUnit :: Name
namePictureUnit = Name ["pictureUnit"]

nameVideoUnit :: Name
nameVideoUnit = Name ["videoUnit"]

nameAcquisitionUnit :: Name
nameAcquisitionUnit = Name ["acquisitionUnit"]

nameRover :: Name
nameRover = Name ["rover"]

c :: Name
c = Name ["c"]

a :: Name
a = Name ["a"]

s :: Name
s = Name ["s"]

p :: Name
p = Name ["p"]

v :: Name
v = Name ["v"]

--
-- Controller
--
controllerUnit :: Component
controllerUnit = BasicComponent nameController sig beh tcs
  where
    m2 = mkMessage "m2" "{urlVid:String,urlPic:String}"
    m3 = mkMessage "m3" "{url:String}"
    run = mkOperation "run"
    sig = Signature [run]
                    [askVid,askPic]
                    (fromList [(run,m1),(askVid,m1),(askPic,m1)])
                    (fromList [(run, Just m2),(askVid, Just m3),(askPic, Just m3)])
    beh = LabelledTransitionSystem
      [receive run, reply run
      ,invoke askVid, result askVid
      ,invoke askPic, result askPic]
      (State <$> ["0","1","2","3","4","5","6"])
      (State "0")
      [State "6"]
      ["0" -| receive run   |-> "1"
      ,"1" -| invoke askVid |-> "2"
      ,"2" -| result askVid |-> "3"
      ,"3" -| invoke askPic |-> "4"
      ,"4" -| result askPic |-> "5"
      ,"5" -| reply run     |-> "6"]
    tcs = [TimeConstraint (receive run) (reply run) 55 60]

--
-- Store Unit
--
storeUnit :: Component
storeUnit = BasicComponent nameStoreUnit sig beh tcs
  where
    sig = Signature [storePic,storeVid]
                    []
                    (fromList [(storePic, m1s), (storeVid, m1s)])
                    (fromList [(storePic, Nothing), (storeVid, Nothing)])
    beh = LabelledTransitionSystem
      [receive storePic, receive storeVid, tau]
      (State <$> ["0","1"])
      (State "0")
      [State "0"]
      ["0" -| receive storePic |-> "1"
      ,"0" -| receive storeVid |-> "1"
      ,"1" -| tau           |-> "0"]
    tcs = []

--
-- Picture Unit
--
pictureUnit :: Component
pictureUnit = BasicComponent namePictureUnit sig beh tcs
  where
    m2 = mkMessage "m2" "{data:RawPicture}"
    sig = Signature [askPic]
                    [getPic, storePic]
                    (fromList [(askPic, m1), (getPic, m1), (storePic, m1s)])
                    (fromList [(askPic, Just m4), (getPic, Just m2), (storePic, Nothing)])
    beh = LabelledTransitionSystem
      [receive askPic, reply askPic
      ,invoke getPic, result getPic
      ,invoke storePic
      ,tau]
      (State <$> ["0","1","2","3","4","5","6"])
      (State "0")
      [State "6"]
      ["0" -| receive askPic  |-> "1"
      ,"1" -| invoke getPic   |-> "2"
      ,"2" -| result getPic   |-> "3"
      ,"3" -| tau             |-> "4"
      ,"3" -| tau             |-> "5"
      ,"4" -| invoke storePic |-> "5"
      ,"5" -| reply askPic    |-> "6"]
    tcs = [TimeConstraint (receive askPic) (reply askPic) 44 46
          ,TimeConstraint (result getPic) (invoke storePic) 0 12
          ,TimeConstraint (invoke getPic) (result getPic) 0 6]

--
-- Video Unit
--
vut1 :: VTransition
vut1 = "0" -| receive askVid  |-> "1"
vut2 :: VTransition
vut2 = "1" -| invoke getVid   |-> "2"
vut3 :: VTransition
vut3 = "2" -| result getVid   |-> "3"
vut4 :: VTransition
vut4 = "3" -| tau             |-> "4"
vut5 :: VTransition
vut5 = "3" -| tau             |-> "5"
vut6 :: VTransition
vut6 = "4" -| invoke storeVid |-> "5"
vut7 :: VTransition
vut7 = "5" -| reply askVid    |-> "6"

vuk1 :: TimeConstraint
vuk1 = TimeConstraint (receive askVid) (reply askVid) 44 46
vuk2 :: TimeConstraint
vuk2 = TimeConstraint (result getVid) (invoke storeVid) 0 12
vuk3 :: TimeConstraint
vuk3 = TimeConstraint (invoke getVid) (result getVid) 0 6

videoUnit :: Component
videoUnit = BasicComponent nameVideoUnit sig beh tcs
  where m2 = mkMessage "m2" "{data:RawVideo}"
        sig =
          Signature [askVid]
                    [getVid,storeVid]
                    (fromList [(askVid,m1),(getVid,m1),(storeVid,m1s)])
                    (fromList [(askVid,Just m4)
                              ,(getVid,Just m2)
                              ,(storeVid,Nothing)])
        beh =
          LabelledTransitionSystem
            [receive askVid
            ,reply askVid
            ,invoke getVid
            ,result getVid
            ,invoke storeVid
            ,tau]
            (State <$> ["0","1","2","3","4","5","6"])
            (State "0")
            [State "6"]
            [vut1,vut2,vut3,vut4,vut5,vut6,vut7]
        tcs = [vuk1,vuk2,vuk3]

--
-- Acquisition Unit
--
acquisitionUnit :: Component
acquisitionUnit = CompositeComponent nameAcquisitionUnit  sig cs inb exb
  where
    m2a = mkMessage "m2a" "{data:RawPicture}"
    m2b = mkMessage "m2b" "{data:RawVideo}"
    sig = Signature [askPic, askVid]
                    [getPic, getVid, storePic, storeVid]
                    (fromList [(askPic, m1), (getPic, m1), (storePic, m1s)
                             ,(askVid, m1), (getVid, m1), (storeVid, m1s)])
                    (fromList [(askPic, Just m4), (getPic, Just m2a), (storePic, Nothing)
                             ,(askVid, Just m4), (getVid, Just m2b), (storeVid, Nothing)])
    cs = [(p, pictureUnit), (v, videoUnit)]
    inb = []
    exb = [self # askPic   <--> p    # askPic
          ,self # askVid   <--> v    # askVid
          ,p    # getPic   <--> self # getPic
          ,v    # getVid   <--> self # getVid
          ,p    # storePic <--> self # storePic
          ,v    # storeVid <--> self # storeVid]

--
-- Rover
--
rover :: Component
rover = CompositeComponent nameRover sig cs inb exb
  where
    m2a = mkMessage "m2a" "{data:RawPicture}"
    m2b = mkMessage "m2b" "{data:RawVideo}"
    run = mkOperation "run"
    store = mkOperation "store"
    sig = Signature [run]
                    [getPic, getVid]
                    (fromList [(run, m1), (getPic, m1), (getVid, m1)])
                    (fromList [(run, Nothing), (getPic, Just m2a), (getVid, Just m2b)])
    cs = [(c, controllerUnit), (a, acquisitionUnit), (s, storeUnit)]
    inb = [c    # askPic   >--< a    # askPic
          ,c    # askVid   >--< c    # askVid
          ,a    # storePic >--< s    # store
          ,a    # storeVid >--< s    # store]
    exb = [self # run      <--> c    # run
          ,a    # getPic   <--> self # getPic
          ,a    # getVid   <--> self # getVid]

--
-- test results
--

videoUnitTA :: VTA
videoUnitTA = TimedAutomaton nameVideoUnit
              (Location <$> ["0","1","2","3","4","5","6"])
              (Location "0")
              clocksVU
              (alphabet . behavior $ videoUnit)
              [Edge (Location "0") (receive askVid) [] [ClockReset c1] (Location "1")
              ,Edge (Location "1") (invoke getVid) [] [ClockReset c3] (Location "2")
              ,Edge (Location "2") (result getVid) [ClockConstraint c3 GE 0] [ClockReset c2] (Location "3")
              ,Edge (Location "3") tau [] [] (Location "4")
              ,Edge (Location "3") tau [] [] (Location "5")
              ,Edge (Location "4") (invoke storeVid) [ClockConstraint c2 GE 0] [] (Location "5")
              ,Edge (Location "5") (reply askVid) [ClockConstraint c1 GE 44] [] (Location "6")
              ,Edge (Location "6") tau [] [] (Location "6")]
              [(Location "1", [ClockConstraint c1 LE 46])
              ,(Location "2", [ClockConstraint c1 LE 46,ClockConstraint c3 LE 6])
              ,(Location "3", [ClockConstraint c1 LE 46,ClockConstraint c2 LE 12])
              ,(Location "4", [ClockConstraint c1 LE 46,ClockConstraint c2 LE 12])
              ,(Location "5", [ClockConstraint c1 LE 46])]
              where
                clocksVU = genClock <$> timeconstraints videoUnit
                c1 = head clocksVU
                c2 = clocksVU !! 1
                c3 = clocksVU !! 2

pictureUnitTA :: VTA
pictureUnitTA = TimedAutomaton namePictureUnit
              (Location <$> ["0","1","2","3","4","5","6"])
              (Location "0")
              clocksPU
              (alphabet . behavior $ pictureUnit)
              [Edge (Location "0") (receive askPic) [] [ClockReset c1] (Location "1")
              ,Edge (Location "1") (invoke getPic) [] [ClockReset c3] (Location "2")
              ,Edge (Location "2") (result getPic) [ClockConstraint c3 GE 0] [ClockReset c2] (Location "3")
              ,Edge (Location "3") tau [] [] (Location "4")
              ,Edge (Location "3") tau [] [] (Location "5")
              ,Edge (Location "4") (invoke storePic) [ClockConstraint c2 GE 0] [] (Location "5")
              ,Edge (Location "5") (reply askPic) [ClockConstraint c1 GE 44] [] (Location "6")
              ,Edge (Location "6") tau [] [] (Location "6")]
              [(Location "1", [ClockConstraint c1 LE 46])
              ,(Location "2", [ClockConstraint c1 LE 46,ClockConstraint c3 LE 6])
              ,(Location "3", [ClockConstraint c1 LE 46,ClockConstraint c2 LE 12])
              ,(Location "4", [ClockConstraint c1 LE 46,ClockConstraint c2 LE 12])
              ,(Location "5", [ClockConstraint c1 LE 46])]
              where
                clocksPU = genClock <$> timeconstraints pictureUnit
                c1 = head clocksPU
                c2 = clocksPU !! 1
                c3 = clocksPU !! 2

storeUnitTA :: VTA
storeUnitTA = TimedAutomaton nameStoreUnit
            (Location <$> ["0","1"])
            (Location "0")
            []
            (alphabet . behavior $ storeUnit)
            [Edge (Location "0") (receive storePic) [] [] (Location "1")
            ,Edge (Location "0") (receive storeVid) [] [] (Location "1")
            ,Edge (Location "1") tau [] [] (Location "0")
            ,Edge (Location "0") tau [] [] (Location "0")]
            []

controllerTA :: VTA
controllerTA = TimedAutomaton nameController
             (Location <$> ["0","1","2","3","4","5","6"])
             (Location "0")
             clocksC
             (alphabet . behavior $ controllerUnit)
             []
             []
             where
              clocksC = genClock <$> timeconstraints controllerUnit
              c1 = head clocksC

roverTAs :: [VTA]
roverTAs = [controllerTA
           ,videoUnitTA
           ,pictureUnitTA
           ,storeUnitTA]

--
-- helpers (to be moved to VECA DSL if useful)
--

mkMessageType :: String -> MessageType
mkMessageType = MessageType

mkMessage :: String -> String -> Message
mkMessage m t = Message (Name [m]) $ mkMessageType t

mkOperation :: String -> Operation
mkOperation o = Operation $ Name [o]

receive :: Operation -> CIOEvent Operation
receive = CReceive

reply :: Operation -> CIOEvent Operation
reply  = CReply

invoke :: Operation -> CIOEvent Operation
invoke = CInvoke

result :: Operation -> CIOEvent Operation
result = CResult

tau :: CIOEvent Operation
tau = CTau

infix 1 |-> --
(|->) :: (a, b) -> a -> Transition b a
(s1, l) |-> s2 = Transition (State s1) l (State s2)

infix 2 -| --
(-|) :: a -> b -> (a, b)
s1 -| l = (s1, l)

infix 2 <--> --
(<-->) :: JoinPoint -> JoinPoint -> Binding
j1 <--> j2 = ExternalBinding j1 j2

infix 2 >--< --
(>--<) :: JoinPoint -> JoinPoint -> Binding
j1 >--< j2 = InternalBinding j1 j2

infix 3 # --
(#) :: Name -> Operation -> JoinPoint
n # o = JoinPoint n o
