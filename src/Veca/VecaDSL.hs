-----------------------------------------------------------------------------
-- |
-- Module      :  Veca.VecaDSL
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- DSL for Veca.
-----------------------------------------------------------------------------

-- links for DSL:
-- https://www.reddit.com/r/haskell/comments/4k930m/dsl_in_haskell/
-- https://www.reddit.com/r/haskell/comments/2e8d53/whats_the_best_practice_for_building_a_dsl_in/

module Veca.VecaDSL (
  (-|)
  ,(|->)
  ,(-:)
  ,(◊)
  ,(<-->)
  ,(>--<)
  ,message
  ,Veca.VecaDSL.operation
  ,tau
  ,receive
  ,reply
  ,invoke
  ,result
  ,provided
  ,required
  ,behaviour
  ,constraints
  ,check
  ,self
  ,subcomponents
  ,internalbindings
  ,externalbindings
  ,basiccomponent
  ,compositecomponent)
where

import           Data.Map                        as M (Map, fromList, (!))
import           Data.Maybe                      as X (isJust)
import           Data.Monoid                     as DM ((<>))
import           Models.LabelledTransitionSystem
import           Numeric.Natural                 as N (Natural)
import           Veca.Veca

data DSL_Operation =
  DSL_Operation {op     :: Operation
                ,inmsg  :: Message
                ,outmsg :: Maybe Message}

infix 2 -| --
(-|) :: Natural -> BehaviorEvent -> (State Natural, BehaviorEvent)
s1 -| e = (State s1,e)

infix 1 |-> --
(|->) :: (State Natural, BehaviorEvent) -> Natural -> Transition BehaviorEvent Natural
(s1,e) |-> s2 = Transition s1 e (State s2)

infix 1 -: --
(-:) :: String -> Component Natural -> (String,Component Natural)
n -: c = (n,c)

infix 2 <--> --
(<-->) :: JoinPoint -> JoinPoint -> Binding
j1 <--> j2 = ExternalBinding j1 j2

infix 2 >--< --
(>--<) :: JoinPoint -> JoinPoint -> Binding
j1 >--< j2 = InternalBinding j1 j2

infix 3 ◊ --
(◊) :: String -> DSL_Operation -> JoinPoint
"self" ◊ o = JoinPoint Self $ op o
n ◊ o = JoinPoint (Name n) $ op o

self :: String
self = "self"

message :: String -> Message
message = Message

operation :: String -> [Message] -> DSL_Operation
operation s (m1:m2:ms) = DSL_Operation (Operation s) m1 (Just m2)
operation s (m1:[])    = DSL_Operation (Operation s) m1 Nothing
operation s _          = DSL_Operation (Operation s) (Message "") Nothing

tau :: BehaviorEvent
tau = CTau

receive :: DSL_Operation -> BehaviorEvent
receive o = CReceive $ op o

reply :: DSL_Operation -> BehaviorEvent
reply o = CReply $ op o

invoke :: DSL_Operation -> BehaviorEvent
invoke o = CInvoke $ op o

result :: DSL_Operation -> BehaviorEvent
result o = CResult $ op o

oreceive :: Operation -> BehaviorEvent
oreceive o = CReceive o

oreply :: Operation -> BehaviorEvent
oreply o = CReply o

oinvoke :: Operation -> BehaviorEvent
oinvoke o = CInvoke o

oresult :: Operation -> BehaviorEvent
oresult o = CResult o

provided :: [DSL_Operation] -> [DSL_Operation] -> Signature
provided os1 os2 =
  Signature {providedOperations = fmap op os1
            ,requiredOperations = fmap op os2
            ,input = M.fromList [(op o,inmsg o)|o <- os]
            ,output = M.fromList [(op o,outmsg o)|o <- os]}
  where os = os1 <> os2

required :: [DSL_Operation] -> [DSL_Operation]
required = id

behaviour :: Signature -> Natural -> [Natural] -> [Transition BehaviorEvent Natural] -> Behavior Natural
behaviour sig s0 fs ts =
  LabelledTransitionSystem (alphabetForSignature sig)
                           ss
                           (State s0)
                           (fmap State fs)
                           ts
  where ss = (fmap source ts) <> (fmap target ts)
        alphabetForSignature
          :: Signature -> [BehaviorEvent]
        alphabetForSignature s =
          concat [
                  -- for each 2-way required operation o, result o
                  [oresult o|o <- requiredOperations s,isJust ((output s) M.! o)]
                 ,
                  -- for each 2-way provided operation o, reply o
                  [oreply o|o <- providedOperations s,isJust ((output s) M.! o)]
                 ,
                  -- for each required operation o, invoke o
                  [oinvoke o|o <- requiredOperations s]
                 ,
                  -- for each provided operation o, receive o
                  [oreceive o|o <- providedOperations s]
                 ,
                  -- tau
                  [CTau]]

constraints :: [TimeConstraint] -> [TimeConstraint]
constraints = id

check :: (DSL_Operation -> BehaviorEvent) -> DSL_Operation -> [Natural] -> (DSL_Operation -> BehaviorEvent) -> DSL_Operation -> TimeConstraint
check f1 e1 r f2 e2 = TimeConstraint (f1 e1) (f2 e2) (minimum r) (maximum r)

basiccomponent :: String
               -> Signature
               -> Behavior Natural
               -> [TimeConstraint]
               -> Component Natural
basiccomponent i s b c = BasicComponent i s b c

subcomponents
  :: [(String,Component Natural)] -> Map Name (Component Natural)
subcomponents l = M.fromList [(Name s,c) | (s,c) <- l]

compositecomponent :: String
                   -> Signature
                   -> Map Name (Component Natural)
                   -> [Binding]
                   -> [Binding]
                   -> Component Natural
compositecomponent i s cs ibs ebs = CompositeComponent i s cs ibs ebs

internalbindings :: [Binding] -> [Binding]
internalbindings = id

externalbindings :: [Binding] -> [Binding]
externalbindings = id
