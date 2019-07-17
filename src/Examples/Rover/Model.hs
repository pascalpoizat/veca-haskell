{-|
Module      : Examples.Rover.Model
Description : Model for the Rover example
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Examples.Rover.Model where

import           Numeric.Natural
import           Data.Map.Strict
import           Models.Events
import           Models.LabelledTransitionSystem
import           Models.Name
import           Veca.Model

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
run :: Operation
run = mkOperation "run"

m1 :: Message
m1 = mkMessage "m1" "{}"
m1s :: Message
m1s = mkMessage "m1s" "{url:String,file:File}"
m4 :: Message
m4 = mkMessage "m4" "{url:String}"

nameController :: VName
nameController = Name ["controller"]

nameStoreUnit :: VName
nameStoreUnit = Name ["storeUnit"]

namePictureUnit :: VName
namePictureUnit = Name ["pictureUnit"]

nameVideoUnit :: VName
nameVideoUnit = Name ["videoUnit"]

nameAcquisitionUnit :: VName
nameAcquisitionUnit = Name ["acquisitionUnit"]

nameRover :: VName
nameRover = Name ["rover"]

c :: VName
c = Name ["c"]

a :: VName
a = Name ["a"]

s :: VName
s = Name ["s"]

p :: VName
p = Name ["p"]

v :: VName
v = Name ["v"]

r :: VName
r = Name ["r"]

n1 :: VName
n1 = Name ["1"]

n2 :: VName
n2 = Name ["2"]

n3 :: VName
n3 = Name ["3"]

n4 :: VName
n4 = Name ["4"]

n5 :: VName
n5 = Name ["5"]

n6 :: VName
n6 = Name ["6"]

n7 :: VName
n7 = Name ["7"]

--
-- Controller
--
controllerUnit :: ComponentInstance
controllerUnit = ComponentInstance c $ BasicComponent nameController sig beh
 where
  m2  = mkMessage "m2" "{urlVid:String,urlPic:String}"
  m3  = mkMessage "m3" "{url:String}"
  sig = Signature
    [run]
    [askVid, askPic]
    (fromList [(run, m1), (askVid, m1), (askPic, m1)])
    (fromList [(run, Just m2), (askVid, Just m3), (askPic, Just m3)])
  beh = LabelledTransitionSystem
    mempty
    [ receive run
    , reply run
    , invoke askVid
    , result askVid
    , invoke askPic
    , result askPic
    ]
    (State <$> ["0", "1", "2", "3", "4", "5", "6"])
    (State "0")
    [State "6"]
    [ "0" -| receive run |-> "1"
    , "1" -| invoke askVid |-> "2"
    , "2" -| result askVid |-> "3"
    , "3" -| invoke askPic |-> "4"
    , "4" -| result askPic |-> "5"
    , "5" -| reply run |-> "6"
    ]

--
-- Store Unit
--
storeUnit :: ComponentInstance
storeUnit = ComponentInstance s $ BasicComponent nameStoreUnit sig beh
 where
  sig = Signature [storePic, storeVid]
                  []
                  (fromList [(storePic, m1s), (storeVid, m1s)])
                  (fromList [(storePic, Nothing), (storeVid, Nothing)])
  beh = LabelledTransitionSystem
    mempty
    [tau (TimeValue 2) (TimeValue 4), receive storePic, receive storeVid]
    (State <$> ["0", "1"])
    (State "0")
    [State "0"]
    [ "0" -| receive storePic |-> "1"
    , "0" -| receive storeVid |-> "1"
    , "1" -| tau (TimeValue 2) (TimeValue 4) |-> "0"
    ]

--
-- Picture Unit
--
pictureUnit :: ComponentInstance
pictureUnit = ComponentInstance p $ BasicComponent namePictureUnit sig beh
 where
  m2  = mkMessage "m2" "{data:RawPicture}"
  sig = Signature
    [askPic]
    [getPic, storePic]
    (fromList [(askPic, m1), (getPic, m1), (storePic, m1s)])
    (fromList [(askPic, Just m4), (getPic, Just m2), (storePic, Nothing)])
  beh = LabelledTransitionSystem
    mempty
    [ receive askPic
    , reply askPic
    , invoke getPic
    , result getPic
    , invoke storePic
    , tau (TimeValue 2) (TimeValue 4)
    ]
    (State <$> ["0", "1", "2", "3", "4", "5", "6"])
    (State "0")
    [State "6"]
    [ "0" -| receive askPic |-> "1"
    , "1" -| invoke getPic |-> "2"
    , "2" -| result getPic |-> "3"
    , "3" -| tau (TimeValue 2) (TimeValue 4) |-> "4"
    , "3" -| tau (TimeValue 2) (TimeValue 4) |-> "5"
    , "4" -| invoke storePic |-> "5"
    , "5" -| reply askPic |-> "6"
    ]

--
-- Video Unit
--
vut1 :: VTransition
vut1 = "0" -| receive askVid |-> "1"
vut2 :: VTransition
vut2 = "1" -| invoke getVid |-> "2"
vut3 :: VTransition
vut3 = "2" -| result getVid |-> "3"
vut4 :: VTransition
vut4 = "3" -| tau (TimeValue 2) (TimeValue 4) |-> "4"
vut5 :: VTransition
vut5 = "3" -| tau (TimeValue 2) (TimeValue 4) |-> "5"
vut6 :: VTransition
vut6 = "4" -| invoke storeVid |-> "5"
vut7 :: VTransition
vut7 = "5" -| reply askVid |-> "6"

videoUnit :: ComponentInstance
videoUnit = ComponentInstance v $ BasicComponent nameVideoUnit sig beh
 where
  m2  = mkMessage "m2" "{data:RawVideo}"
  sig = Signature
    [askVid]
    [getVid, storeVid]
    (fromList [(askVid, m1), (getVid, m1), (storeVid, m1s)])
    (fromList [(askVid, Just m4), (getVid, Just m2), (storeVid, Nothing)])
  beh = LabelledTransitionSystem
    mempty
    [ receive askVid
    , reply askVid
    , invoke getVid
    , result getVid
    , invoke storeVid
    , tau (TimeValue 2) (TimeValue 4)
    ]
    (State <$> ["0", "1", "2", "3", "4", "5", "6"])
    (State "0")
    [State "6"]
    [vut1, vut2, vut3, vut4, vut5, vut6, vut7]

--
-- Acquisition Unit
--
acquisitionUnit :: ComponentInstance
acquisitionUnit = ComponentInstance a
  $ CompositeComponent nameAcquisitionUnit sig cs inb exb
 where
  m2a = mkMessage "m2a" "{data:RawPicture}"
  m2b = mkMessage "m2b" "{data:RawVideo}"
  sig = Signature
    [askPic, askVid]
    [getPic, getVid, storePic, storeVid]
    (fromList
      [ (askPic  , m1)
      , (getPic  , m1)
      , (storePic, m1s)
      , (askVid  , m1)
      , (getVid  , m1)
      , (storeVid, m1s)
      ]
    )
    (fromList
      [ (askPic  , Just m4)
      , (getPic  , Just m2a)
      , (storePic, Nothing)
      , (askVid  , Just m4)
      , (getVid  , Just m2b)
      , (storeVid, Nothing)
      ]
    )
  cs  = [pictureUnit, videoUnit]
  inb = []
  exb =
    [ n1 @: self # askPic ==> p # askPic
    , n2 @: self # askVid ==> v # askVid
    , n3 @: p # getPic ==> self # getPic
    , n4 @: v # getVid ==> self # getVid
    , n5 @: p # storePic ==> self # storePic
    , n6 @: v # storeVid ==> self # storeVid
    ]

--
-- Rover
--
rover :: ComponentInstance
rover = ComponentInstance r (CompositeComponent nameRover sig cs inb exb)
 where
  m2a   = mkMessage "m2a" "{data:RawPicture}"
  m2b   = mkMessage "m2b" "{data:RawVideo}"
  store = mkOperation "store"
  sig   = Signature
    [run]
    [getPic, getVid]
    (fromList [(run, m1), (getPic, m1), (getVid, m1)])
    (fromList [(run, Nothing), (getPic, Just m2a), (getVid, Just m2b)])
  cs = [controllerUnit, acquisitionUnit, storeUnit]
  inb =
    [ n1 @: c # askPic --> a # askPic
    , n2 @: c # askVid --> a # askVid
    , n3 @: a # storePic --> s # storePic
    , n4 @: a # storeVid --> s # storeVid
    ]
  exb =
    [ n5 @: self # run ==> c # run
    , n6 @: a # getPic ==> self # getPic
    , n7 @: a # getVid ==> self # getVid
    ]

--
-- helpers (to be moved to VECA DSL if useful)
--

mkMessageType :: String -> MessageType
mkMessageType = MessageType

mkMessage :: String -> String -> Message
mkMessage m t = Message (Name [m]) $ mkMessageType t

mkOperation :: String -> Operation
mkOperation o = Operation $ Name [o]

receive :: Operation -> VLabel
receive = EventLabel . CReceive

reply :: Operation -> VLabel
reply = EventLabel . CReply

invoke :: Operation -> VLabel
invoke = EventLabel . CInvoke

result :: Operation -> VLabel
result = EventLabel . CResult

tau :: TimeStep -> TimeStep -> VLabel
tau = InternalLabel

theta :: Timeout -> VLabel
theta = TimeoutLabel

infix 1 |-> --
(|->) :: (a, b) -> a -> Transition b a
(s1, l) |-> s2 = Transition (State s1) l (State s2)

infix 2 -| --
(-|) :: a -> b -> (a, b)
s1 -| l = (s1, l)

infix 2 ==> --
(==>) :: (VName, JoinPoint) -> JoinPoint -> Binding
(i, j1) ==> j2 = Binding External i j1 j2

infix 2 --> --
(-->) :: (VName, JoinPoint) -> JoinPoint -> Binding
(i, j1) --> j2 = Binding Internal i j1 j2

infix 4 # --
(#) :: VName -> Operation -> JoinPoint
n # o = JoinPoint n o

infix 3 @: --
(@:) :: VName -> JoinPoint -> (VName, JoinPoint)
i @: j = (i, j)
