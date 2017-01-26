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

import           Data.Either      as E
import           Data.Map         as M (Map, fromList, keys, toList)
import           Veca             (Behavior, BehaviorEvent, Component, Message,
                                   Operation, TimeConstraint, isValidComponent)

roverTests :: TestTree
roverTests = testGroup "Tests" [unittests]

unittests :: TestTree
unittests =
  testGroup "Unit tests for the Rover Case Study"
            [u_controller
            ,u_storeunit
            ,u_pictureunit
            ,u_videounit
            ,u_acquisitionunit]

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
controller :: Component
controller =
  basic .
  --
  message "m1" "{}" .
  message "m2" "{urlVid:String,urlPic:String}" .
  message "m3" "{url:String}" .
  --
  operation "run"    ["m1","m2"] .
  operation "askVid" ["m1","m3"] .
  operation "askPic" ["m1","m3"] .
  --
  provided ["run"            ] .
  required ["askVid","askPic"] .
  --
  begin_behaviour .
  initial 0 .
  final [6] .
  0 -| receive "run"    |-> 1 .
  1 -| invoke  "askVid" |-> 2 .
  2 -| result  "askVid" |-> 3 .
  3 -| invoke  "askPic" |-> 4 .
  4 -| result  "askPic" |-> 5 .
  5 -| reply   "run"    |-> 6 .
  end_behavior .
  --
  check receive "run" [55 .. 60] reply "run" .
  --
  end

--
-- Store Unit
--
storeUnit :: Component
storeUnit =
  basic .
  --
  message "m1" "{url:String,file:File}" .
  --
  operation "store" ["m1"] .
  --
  provided ["store"] .
  --
  begin_behavior .
  initial 0 .
  final [0] .
  0 -| receive "store" |-> 1 .
  1 -| tau             |-> 0 .
  end_behavior .
  --
  end

--
-- Picture Unit
--
pictureUnit :: Component
pictureUnit =
  basic .
  --
  message "m1" "{}" .
  message "m2" "{data:RawPicture}" .
  message "m3" "{url:String,file:File}" .
  message "m4" "{url:String}" .
  --
  operation "askPic"   ["m1","m4"] .
  operation "getPic"   ["m1","m2"] .
  operation "storePic" ["m3"     ] .
  --
  provided ["askPic"           ] .
  required ["getPic","storePic"] .
  --
  begin_behaviour .
  initial 0 .
  final [6] .
  0 -| receive "askPic"   |-> 1 .
  1 -| invoke  "getPic"   |-> 2 .
  2 -| result  "getPic"   |-> 3 .
  3 -| tau                |-> 4 .
  3 -| tau                |-> 5 .
  4 -| invoke  "storePic" |-> 5 .
  5 -| reply   "askPic"   |-> 6 .
  end_behavior .
  --
  check receive "askPic" [44 .. 46] reply   "askPic" .
  check result  "getPic" [ 0 .. 12] invoke  "storePic" .
  check invoke  "getPic" [ 0 ..  6] results "getPic" .
  --
  end

--
-- Video Unit
--
videoUnit :: Component
videoUnit =
  basic .
  --
  message "m1" "{}" .
  message "m2" "{data:RawVideo}" .
  message "m3" "{url:String,file:File}" .
  message "m4" "{url:String}" .
  --
  operation "askVid"   ["m1","m4"] .
  operation "getVid"   ["m1","m2"] .
  operation "storeVid" ["m3"     ] .
  --
  provided ["askVid"           ] .
  required ["getVid","storeVid"] .
  --
  begin_behaviour .
  initial 0 .
  final [6] .
  0 -| receive "askVid"   |-> 1 .
  1 -| invoke  "getVid"   |-> 2 .
  2 -| result  "getVid"   |-> 3 .
  3 -| tau                |-> 4 .
  3 -| tau                |-> 5 .
  4 -| invoke  "storeVid" |-> 5 .
  5 -| reply   "askVid"   |-> 6 .
  end_behavior .
  --
  check receive "askVid" [44 .. 46] reply   "askVid" .
  check result  "getVid" [ 0 .. 12] invoke  "storeVid" .
  check invoke  "getVid" [ 0 ..  6] results "getVid" .
  --
  end

--
-- Acquisition Unit
--
acquisitionUnit :: Component
acquisitionUnit =
  composite .
  --
  message "m1"  "{}" .
  message "m2a" "{data:RawPicture}" .
  message "m2b" "{data:RawVideo}" .
  message "m3"  "{url:String,file:File}" .
  message "m4"  "{url:String}" .
  --
  operation "askPic"   ["m1", "m4"] .
  operation "getPic"   ["m1","m2a"] .
  operation "storePic" ["m3"      ] .
  operation "askVid"   ["m1", "m4"] .
  operation "getVid"   ["m1","m2b"] .
  operation "storeVid" ["m3"      ] .
  --
  provided ["askPic","askVid"                      ] .
  required ["getPic","getVid","storePic","storeVid"] .
  --
  child "p" pictureUnit .
  child "v" videoUnit .
  --
  bind "self" ◊ "askPic"   <--> "p"    ◊ "askPic"   .
  bind "self" ◊ "askVid"   <--> "v"    ◊ "askVid"   .
  bind "p"    ◊ "getPic"   <--> "self" ◊ "getPic"   .
  bind "v"    ◊ "getVid"   <--> "self" ◊ "getVid"   .
  bind "p"    ◊ "storePic" <--> "self" ◊ "storePic" .
  bind "v"    ◊ "storeVid" <--> "self" ◊ "storeVid" .
  --
  end

--
-- Rover
--
rover :: Component
rover =
  composite .
  --
  message "m1"  "{}" .
  message "m2a" "{data:RawPicture}" .
  message "m2b" "{data:RawVideo}"   .
  --
  operation "run"    ["m1"      ] .
  operation "getPic" ["m1","m2a"] .
  operation "getVid" ["m1","m2b"] .
  --
  provided ["run"            ] .
  required ["getPic","getVid"] .
  --
  child "c" controller .
  child "a" acquisitionUnit .
  child "s" storeUnit .
  --
  bind "c"    ◊ "askPic"   >--< "a"    ◊ "askPic" .
  bind "c"    ◊ "askVid"   >--< "c"    ◊ "askVid" .
  bind "a"    ◊ "storePic" >--< "s"    ◊ "store"  .
  bind "a"    ◊ "storeVid" >--< "s"    ◊ "store"  .
  bind "self" ◊ "run"      <--> "c"    ◊ "run"    .
  bind "a"    ◊ "getPic"   <--> "self" ◊ "getPic" .
  bind "a"    ◊ "getVid"   <--> "self" ◊ "getVid" .
  --
  end

--
-- DSL
--

data DSL_Builder =
  DSL_Builder {messages         :: [DSL_Message]
              ,operations       :: [DSL_Operation]
              ,provides         :: [String]
              ,requires         :: [String]
              ,behavior         :: [DSL_Behavior]
              ,constraints      :: [TimeConstraint]
              ,children         :: [(String,Component)]
              ,internalbindings :: [DSL_Binding]
              ,externalbindings :: [DSL_Binding]
              }

data DSL_Message =
  DSL_Message {n :: String
              ,v :: String}

data DSL_Operation =
  DSL_Operation {op     :: Operation
                ,inmsg  :: Maybe Message
                ,outmsg :: Maybe Message}

data DSL_Behavior =
  DSL_Behavior {s0  :: Maybe String
               ,sfs :: Maybe [String]
               ,ts  :: Maybe [DSL_Transition]}

data DSL_Transition =
  DSL_Transition {s1 :: String
                 ,f  :: DSL_Operation -> BehaviorEvent
                 ,o  :: String
                 ,s2 :: String}

data DSL_Constraint =
  DSL_Constraint {f1 :: DSL_Operation -> BehaviorEvent
                 ,o1 :: String
                 ,f2 :: DSL_Operation -> BehaviorEvent
                 ,o2 :: String
                 ,r  :: [Int]}

data DSL_JoinPoint =
  DSL_JoinPoint {scn :: String
                ,sco :: String}

data DSL_BindingKind = Internal | External

data DSL_Binding =
  DSL_Binding {kind :: DSL_BindingKind
               ,j1  :: DSL_JoinPoint
               ,j2  :: DSL_JoinPoint}

--

basic :: DSL_Builder -> Component

composite :: DSL_Builder -> Component

--

infix 3 ◊ --
(◊) :: String -> String -> DSL_JoinPoint
n ◊ o = (n,o)

infix 2 <--> --
(<-->) :: DSL_JoinPoint -> DSL_JoinPoint -> DSL_Binding
j1 <--> j2 = (j1,j2)

infix 2 >--< --
(>--<) :: DSL_JoinPoint -> DSL_JoinPoint -> DSL_Binding
j1 >--< j2 = (j1,j2)

-- for all

message :: String -> String -> DSL_Builder -> DSL_Builder
message n d (DSL_Builder ms x1 x2 x3 x4 x5 x6 x7 x8) = DSL_Builder (DSL_Message n d):ms x1 x2 x3 x4 x5 x6 x7 x8

operation :: String -> [String] -> DSL_Builder -> DSL_Builder
operation n (m1:m2:_) (DSL_Builder x1 os x2 x3 x4 x5 x6 x7 x8) = DSL_Builder x1 (DSL_Operation n m1 $ Just m2):os x2 x3 x4 x5 x6 x7 x8
operation n (m1:[]) (DSL_Builder x1 os x2 x3 x4 x5 x6 x7 x8) = DSL_Builder x1 (DSL_Operation n m1 Nothing):os x2 x3 x4 x5 x6 x7 x8
operation n ([]) (DSL_Builder x1 os x2 x3 x4 x5 x6 x7 x8) = DSL_Builder x1 (DSL_Opertion n Nothing Nothing):os x2 x3 x4 x5 x6 x7 x8

provided :: [String] -> DSL_Builder -> DSL_Builder
provided os (DSL_Builder x1 x2 ps x3 x4 x5 x6 x7 x8) = DSL_Builder x1 x2 os++ps x3 x4 x5 x6 x7 x8

required :: [String] -> DSL_Builder -> DSL_Builder
required os (DSL_Builder x1 x2 x3 rs x4 x5 x6 x7 x8) = DSL_Builder x1 x2 x3 os++rs x4 x5 x6 x7 x8

end :: DSL_Builder
end = DSL_Builder [] [] [] [] [] [] [] [] []

-- for composites

child :: String -> Component -> DSL_Builder -> DSL_Builder
child n c (DSL_Builder x1 x2 x3 x4 x5 x6 cs x7 x8) = DSL_Builder x1 x2 x3 x4 x5 x6 (n,c)::cs x7 x8

bind :: DSL_Binding -> DSL_Builder -> DSL_Builder
bind b@(DSL_Binding j1 j2) (DSL_Builder x1 x2 x3 x4 x5 x6 x7 ibs x8) = DSL_Builder x1 x2 x3 x4 x5 x6 x7 b:ibs x8
bind b@(DSL_Binding j1 j2) (DSL_Builder x1 x2 x3 x4 x5 x6 x7 x8 ebs) = DSL_Builder x1 x2 x3 x4 x5 x6 x7 x8 b:ebs

-- for basics

begin_behavior :: (DSL_Builder, DSL_Behavior) -> DSL_Builder
begin_behavior ((DSL_Builder x1 x2 x3 x4 _ x5 x6 x7 x8), b) = DSL_Builder x1 x2 x3 x4 b x5 x6 x7 x8

end_behavior :: DSL_Builder -> (DSL_Builder, DSL_Behavior)
end_behavior b = (b, DSL_Behavior Nothing Nothing Nothing)

check :: (DSL_Operation -> BehaviorEvent)
      -> String
      -> [Int]
      -> (DSL_Operation -> BehaviorEvent)
      -> String
      -> DSL_Constraint
check f1 e1 r f2 e2 = TimeConstraint (f1 e1) (f2 e2) (minimum r) (maximum r)
