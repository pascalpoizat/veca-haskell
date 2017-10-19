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
import           Data.Map                        as M (fromList)
import           Models.Events
import           Models.LabelledTransitionSystem
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
            ,testCase "TA generation for the Video Unit (standalone)" $
             cToTA videoUnit @?= videoUnitTA]

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
-- The Rover Case Study
--

askVid :: Operation
askVid = mkOperation "askVid"
getVid :: Operation
getVid = mkOperation "getVid"
storeVid :: Operation
storeVid = mkOperation "storeVid"

--
-- Controller
--
controller :: Component 
controller = BasicComponent "controller" sig beh tcs
  where
    m1 = mkMessage "m1" "{}"
    m2 = mkMessage "m2" "{urlVid:String,urlPic:String}"
    m3 = mkMessage "m3" "{url:String}"
    run = mkOperation "run"
    askPic = mkOperation "askPic"
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
storeUnit = BasicComponent "storeUnit" sig beh tcs
  where
    m1 = mkMessage "m1" "{url:String,file:File}"
    store = mkOperation "store"
    sig = Signature [store]
                    []
                    (fromList [(store, m1)])
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
pictureUnit = BasicComponent "pictureUnit" sig beh tcs
  where
    m1 = mkMessage "m1" "{}"
    m2 = mkMessage "m2" "{data:RawPicture}"
    m3 = mkMessage "m3" "{url:String,file:File}"
    m4 = mkMessage "m4" "{url:String}"
    askPic = mkOperation "askPic"
    getPic = mkOperation "getPic"
    storePic = mkOperation "storePic"
    sig = Signature [askPic]
                    [getPic, storePic]
                    (fromList [(askPic, m1), (getPic, m1), (storePic, m3)])
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
videoUnit = BasicComponent "videoUnit" sig beh tcs
  where
    m1 = mkMessage "m1" "{}"
    m2 = mkMessage "m2" "{data:RawVideo}"
    m3 = mkMessage "m3" "{url:String,file:File}"
    m4 = mkMessage "m4" "{url:String}"
    sig = Signature [askVid]
                    [getVid, storeVid]
                    (fromList [(askVid, m1), (getVid, m1), (storeVid, m3)])
                    (fromList [(askVid, Just m4), (getVid, Just m2), (storeVid, Nothing)])
    beh = LabelledTransitionSystem
      [receive askVid, reply askVid
      ,invoke getVid, result getVid
      ,invoke storeVid
      ,tau]
      (State <$> ["0","1","2","3","4","5","6"])
      (State "0")
      [State "6"]
      ["0" -| receive askVid  |-> "1"
      ,"1" -| invoke getVid   |-> "2"
      ,"2" -| result getVid   |-> "3"
      ,"3" -| tau             |-> "4"
      ,"3" -| tau             |-> "5"
      ,"4" -| invoke storeVid |-> "5"
      ,"5" -| reply askVid    |-> "6"]
    tcs = [TimeConstraint (receive askVid) (reply askVid) 44 46
          ,TimeConstraint (result getVid) (invoke storeVid) 0 12
          ,TimeConstraint (invoke getVid) (result getVid) 0 6]

--
-- Acquisition Unit
--
acquisitionUnit :: Component
acquisitionUnit = CompositeComponent "acquisitionUnit"  sig cs inb exb
  where
    m1 = mkMessage "m1" "{}"
    m2a = mkMessage "m2a" "{data:RawPicture}"
    m2b = mkMessage "m2b" "{data:RawVideo}"
    m3 = mkMessage "m3" "{url:String,file:File}"
    m4 = mkMessage "m4" "{url:String}"
    askPic = mkOperation "askPic"
    getPic = mkOperation "getPic"
    storePic = mkOperation "storePic"
    p = Name "p"
    v = Name "v"
    sig = Signature [askPic, askVid]
                    [getPic, getVid, storePic, storeVid]
                    (fromList [(askPic, m1), (getPic, m1), (storePic, m3)
                             ,(askVid, m1), (getVid, m1), (storeVid, m3)])
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
rover = CompositeComponent "rover" sig cs inb exb
  where
    m1 = mkMessage "m1"  "{}"
    m2a = mkMessage "m2a" "{data:RawPicture}"
    m2b = mkMessage "m2b" "{data:RawVideo}"
    run = mkOperation "run"
    getPic = mkOperation "getPic"
    askPic = mkOperation "askPic"
    storePic = mkOperation "storePic"
    store = mkOperation "store"
    c = Name "c"
    a = Name "a"
    s = Name "s"
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
videoUnitTA = TimedAutomaton "videoUnit"
              (Location <$> ["0","1","2","3","4","5","6"])
              (Location "0")
              (genClock <$> timeconstraints videoUnit)
              (alphabet . behavior $ videoUnit)
              [Edge (Location "0") (receive askVid) [] [] (Location "1")
              ,Edge (Location "1") (invoke getVid) [] [] (Location "2")
              ,Edge (Location "2") (result getVid) [] [] (Location "3")
              ,Edge (Location "3") tau [] [] (Location "4")
              ,Edge (Location "3") tau [] [] (Location "5")
              ,Edge (Location "4") (invoke storeVid) [] [] (Location "5")
              ,Edge (Location "5") (reply askVid) [] [] (Location "6")
              ,Edge (Location "6") tau [] [] (Location "6")]
              (fromList []) 

--
-- helpers (to be moved to VECA DSL if useful)
--

self :: Name
self = Self

mkMessageType :: String -> MessageType
mkMessageType = MessageType

mkMessage :: String -> String -> Message
mkMessage m t = Message (Name m) $ mkMessageType t

mkOperation :: String -> Operation
mkOperation o = Operation $ Name o

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
