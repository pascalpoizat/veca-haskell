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
import           Test.Tasty.ExpectedFailure
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
            --,uAcquisitionUnit
            --,uRover
            ]

uController :: TestTree
uController =
  testGroup "Unit tests for Controller"
            [testCase "basic component definition is valid" $
              isValidComponent controller @?= True]

uStoreUnit :: TestTree
uStoreUnit =
  testGroup "Unit tests for Store Unit"
            [testCase "basic component definition is valid" $
              isValidComponent storeUnit @?= True]

uPictureUnit :: TestTree
uPictureUnit =
  testGroup "Unit tests for Picture Unit"
            [testCase "basic component definition is valid" $
              isValidComponent pictureUnit @?= True]

uVideoUnit :: TestTree
uVideoUnit =
  testGroup "Unit tests for Video Unit"
            [testCase "basic component definition is valid" $
             isValidComponent videoUnit @?= True
            ,testCase "paths" $
             S.fromList (paths (behavior videoUnit)) @?= S.fromList resPathVU
            ,expectFail (testCase "TA generation for the Video Unit (standalone)" $
             cToTA videoUnit @?= videoUnitTA)]

uAcquisitionUnit :: TestTree
uAcquisitionUnit =
  testGroup "Unit tests for Acquisition Unit"
            [testCase "composite component definition is valid" $
              isValidComponent acquisitionUnit @?= True
            ,testCase "TA generation for the Acquisition Unit (standalone)" $
             cToTA acquisitionUnit @?= undefined]

uRover :: TestTree
uRover =
  testGroup "Unit tests for Rover"
            [testCase "composite component definition is valid" $
              isValidComponent rover @?= True
            ,testCase "TA generation for the Rover (standalone)" $
             cToTA rover @?= undefined]

--
-- Results
--

resPathVU :: [VPath]
resPathVU = Path <$> [[],[t1],[t1,t2],[t1,t2,t3],[t1,t2,t3,t4],[t1,t2,t3,t5],[t1,t2,t3,t4,t6],[t1,t2,t3,t4,t6,t7],[t1,t2,t3,t5,t7]]

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

t1 :: VTransition
t1 = "0" -| receive askVid  |-> "1"

t2 :: VTransition
t2 = "1" -| invoke getVid   |-> "2"

t3 :: VTransition
t3 = "2" -| result getVid   |-> "3"

t4 :: VTransition
t4 = "3" -| tau             |-> "4"

t5 :: VTransition
t5 = "3" -| tau             |-> "5"

t6 :: VTransition
t6 = "4" -| invoke storeVid |-> "5"

t7 :: VTransition
t7 = "5" -| reply askVid    |-> "6"

--
-- Controller
--
controller :: Component
controller = BasicComponent nameController sig beh tcs
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
    store = mkOperation "store"
    sig = Signature [store]
                    []
                    (fromList [(store, m1s)])
                    (fromList [(store, Nothing)])
    beh = LabelledTransitionSystem
      [receive store, tau]
      (State <$> ["0","1"])
      (State "0")
      [State "0"]
      ["0" -| receive store |-> "1"
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
videoUnit :: Component
videoUnit = BasicComponent nameVideoUnit sig beh tcs
  where
    m2 = mkMessage "m2" "{data:RawVideo}"
    sig = Signature [askVid]
                    [getVid, storeVid]
                    (fromList [(askVid, m1), (getVid, m1), (storeVid, m1s)])
                    (fromList [(askVid, Just m4), (getVid, Just m2), (storeVid, Nothing)])
    beh = LabelledTransitionSystem
      [receive askVid, reply askVid
      ,invoke getVid, result getVid
      ,invoke storeVid
      ,tau]
      (State <$> ["0","1","2","3","4","5","6"])
      (State "0")
      [State "6"]
      [t1,t2,t3,t4,t5,t6,t7]
    tcs = [TimeConstraint (receive askVid) (reply askVid) 44 46
          ,TimeConstraint (result getVid) (invoke storeVid) 0 12
          ,TimeConstraint (invoke getVid) (result getVid) 0 6]

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
    cs = [(c, controller), (a, acquisitionUnit), (s, storeUnit)]
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

-- TODO: ONGOING

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
              []
              where
                clocksVU = genClock <$> timeconstraints videoUnit
                c1 = head clocksVU
                c2 = clocksVU !! 1
                c3 = clocksVU !! 2

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
