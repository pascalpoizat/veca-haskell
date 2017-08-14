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

import           Data.Map         as M (fromList)
import           Models.LabelledTransitionSystem
import           Models.Events
import           Veca.Veca
import           Numeric.Natural

roverTests :: TestTree
roverTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests for the Rover Case Study"
            [u_controller
            ,u_storeunit
            ,u_pictureunit
            ,u_videounit
            ,u_acquisitionunit
            ,u_rover]

u_controller :: TestTree
u_controller =
  testGroup "Unit tests for Controller"
            [(testCase "basic component definition is valid" $
              isValidComponent controller @?= True)]

u_storeunit :: TestTree
u_storeunit =
  testGroup "Unit tests for Store Unit"
            [(testCase "basic component definition is valid" $
              isValidComponent storeUnit @?= True)]

u_pictureunit :: TestTree
u_pictureunit =
  testGroup "Unit tests for Picture Unit"
            [(testCase "basic component definition is valid" $
              isValidComponent pictureUnit @?= True)]

u_videounit :: TestTree
u_videounit =
  testGroup "Unit tests for Video Unit"
            [(testCase "basic component definition is valid" $
             isValidComponent videoUnit @?= True)]

u_acquisitionunit :: TestTree
u_acquisitionunit =
  testGroup "Unit tests for Acquisition Unit"
            [(testCase "composite component definition is valid" $
              isValidComponent acquisitionUnit @?= True)]

u_rover :: TestTree
u_rover =
  testGroup "Unit tests for Acquisition Unit"
            [(testCase "composite component definition is valid" $
              isValidComponent rover @?= True)]

--
-- The Rover Case Study
--

--
-- Controller
--
controller :: Component Natural
controller = BasicComponent "controller" sig beh tcs
  where
    m1 = mkMessage "m1" "{}"
    m2 = mkMessage "m2" "{urlVid:String,urlPic:String}"
    m3 = mkMessage "m3" "{url:String}"
    run = mkOperation "run"
    askVid = mkOperation "askVid"
    askPic = mkOperation "askPic"
    sig = Signature [run]
                    [askVid,askPic]
                    (fromList [(run,m1),(askVid,m1),(askPic,m1)])
                    (fromList [(run, Just m2),(askVid, Just m3),(askPic, Just m3)])
    beh = LabelledTransitionSystem
      [receive run, reply run
      ,invoke askVid, result askVid
      ,invoke askPic, result askPic]
      (State <$> [0..6])
      (State 0)
      [(State 6)]
      [0 -| receive run   |-> 1
      ,1 -| invoke askVid |-> 2
      ,2 -| result askVid |-> 3
      ,3 -| invoke askPic |-> 4
      ,4 -| result askPic |-> 5
      ,5 -| reply run     |-> 6]
    tcs = [TimeConstraint (receive run) (reply run) 55 60]

--
-- Store Unit
--
storeUnit :: Component Natural
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
      (State <$> [0..1])
      (State 0)
      [(State 0)]
      [0 -| receive store |-> 1
      ,1 -| tau           |-> 0]
    tcs = []

--
-- Picture Unit
--
pictureUnit :: Component Natural
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
      (State <$> [0..6])
      (State 0)
      [(State 6)]
      [0 -| receive askPic  |-> 1
      ,1 -| invoke getPic   |-> 2
      ,2 -| result getPic   |-> 3
      ,3 -| tau             |-> 4
      ,3 -| tau             |-> 5
      ,4 -| invoke storePic |-> 5
      ,5 -| reply askPic    |-> 6]
    tcs = [TimeConstraint (receive askPic) (reply askPic) 44 46
          ,TimeConstraint (result getPic) (invoke storePic) 0 12
          ,TimeConstraint (invoke getPic) (result getPic) 0 6]

--
-- Video Unit
--
videoUnit :: Component Natural
videoUnit = BasicComponent "videoUnit" sig beh tcs
  where
    m1 = mkMessage "m1" "{}"
    m2 = mkMessage "m2" "{data:RawVideo}"
    m3 = mkMessage "m3" "{url:String,file:File}"
    m4 = mkMessage "m4" "{url:String}"
    askVid = mkOperation "askVid"
    getVid = mkOperation "getVid"
    storeVid = mkOperation "storeVid"
    sig = Signature [askVid]
                    [getVid, storeVid]
                    (fromList [(askVid, m1), (getVid, m1), (storeVid, m3)])
                    (fromList [(askVid, Just m4), (getVid, Just m2), (storeVid, Nothing)])
    beh = LabelledTransitionSystem
      [receive askVid, reply askVid
      ,invoke getVid, result getVid
      ,invoke storeVid
      ,tau]
      (State <$> [0..6])
      (State 0)
      [(State 6)]
      [0 -| receive askVid  |-> 1
      ,1 -| invoke getVid   |-> 2
      ,2 -| result getVid   |-> 3
      ,3 -| tau             |-> 4
      ,3 -| tau             |-> 5
      ,4 -| invoke storeVid |-> 5
      ,5 -| reply askVid    |-> 6]
    tcs = [TimeConstraint (receive askVid) (reply askVid) 44 46
          ,TimeConstraint (result getVid) (invoke storeVid) 0 12
          ,TimeConstraint (invoke getVid) (result getVid) 0 6]

--
-- Acquisition Unit
--
acquisitionUnit :: Component Natural
acquisitionUnit = CompositeComponent "acquisitionUnit"  sig cs inb exb
  where
    m1 = mkMessage "m1" "{}"
    m2a = mkMessage "m2a" "{data:RawPicture}"
    m2b = mkMessage "m2b" "{data:RawVideo}"
    m3 = mkMessage "m3" "{url:String,file:File}"
    m4 = mkMessage "m4" "{url:String}"
    askPic = mkOperation "askPic"
    askVid = mkOperation "askVid"
    getPic = mkOperation "getPic"
    getVid = mkOperation "getVid"
    storePic = mkOperation "storePic"
    storeVid = mkOperation "storeVid"
    p = Name "p"
    v = Name "v"
    sig = Signature [askPic, askVid]
                    [getPic, getVid, storePic, storeVid]
                    (fromList [(askPic, m1), (getPic, m1), (storePic, m3)
                             ,(askVid, m1), (getVid, m1), (storeVid, m3)])
                    (fromList [(askPic, Just m4), (getPic, Just m2a), (storePic, Nothing)
                             ,(askVid, Just m4), (getVid, Just m2b), (storeVid, Nothing)])
    cs = fromList [(p, pictureUnit), (v, videoUnit)]
    inb = []
    exb = [(JoinPoint self askPic) <--> (JoinPoint p $ mkOperation "askPic")
          ,(JoinPoint self askVid) <--> (JoinPoint v $ mkOperation "askVid")
          ,(JoinPoint p $ mkOperation "getPic") <--> (JoinPoint self getPic)
          ,(JoinPoint v $ mkOperation "getVid") <--> (JoinPoint self getVid)
          ,(JoinPoint p $ mkOperation "storePic") <--> (JoinPoint self storePic)
          ,(JoinPoint v $ mkOperation "storeVid") <--> (JoinPoint self storeVid)]

--
-- Rover
--
rover :: Component Natural
rover = CompositeComponent "rover" sig cs inb exb
  where
    m1 = mkMessage "m1"  "{}"
    m2a = mkMessage "m2a" "{data:RawPicture}"
    m2b = mkMessage "m2b" "{data:RawVideo}"
    run = mkOperation "run"
    getPic = mkOperation "getPic"
    getVid = mkOperation "getVid"
    c = Name "c"
    a = Name "a"
    s = Name "s"
    sig = Signature [run]
                    [getPic, getVid]
                    (fromList [(run, m1), (getPic, m1), (getVid, m1)])
                    (fromList [(run, Nothing), (getPic, Just m2a), (getVid, Just m2b)])
    cs = fromList [(c, controller), (a, acquisitionUnit), (s, storeUnit)]
    inb = [(JoinPoint c $ mkOperation "askPic") >--< (JoinPoint a $ mkOperation "askPic")
          ,(JoinPoint c $ mkOperation "askVid") >--< (JoinPoint c $ mkOperation "askVid")
          ,(JoinPoint a $ mkOperation "storePic") >--< (JoinPoint s $ mkOperation "store")
          ,(JoinPoint a $ mkOperation "storeVid") >--< (JoinPoint s $ mkOperation "store")]
    exb = [(JoinPoint self run) <--> (JoinPoint c $ mkOperation "run")
          ,(JoinPoint a $ mkOperation "getPic") <--> (JoinPoint self getPic)
          ,(JoinPoint a $ mkOperation "getVid") <--> (JoinPoint self getVid)]

--
-- makers (to be moved to VECA DSL if useful)
--

self :: Name
self = Self

mkMessageType :: String -> MessageType
mkMessageType t = MessageType t

mkMessage :: String -> String -> Message
mkMessage m t = Message (Name m) $ mkMessageType t

mkOperation :: String -> Operation
mkOperation o = Operation $ Name o

receive :: Operation -> CIOEvent Operation
receive o = CReceive o

reply :: Operation -> CIOEvent Operation
reply o = CReply o

invoke :: Operation -> CIOEvent Operation
invoke o = CInvoke o

result :: Operation -> CIOEvent Operation
result o = CResult o

tau :: CIOEvent Operation
tau = CTau

infix 1 |-> --
(|->) :: (a, b) -> a -> (Transition b a)
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
