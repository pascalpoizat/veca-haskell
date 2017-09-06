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

import           Data.Map                        (Map, fromList, (!))
import           Data.Maybe                      (isJust)
import           Data.Monoid                     ((<>))
import           Models.Events                   (CIOEvent (..))
import           Models.LabelledTransitionSystem (LabelledTransitionSystem (..),
                                                  State (..), Transition (..))
import           Numeric.Natural                 as N (Natural)
import           Veca.Veca

data DSLOperation =
  DSLOperation {op     :: Operation
                ,inmsg  :: Message
                ,outmsg :: Maybe Message}

infix 2 -| --
(-|) :: Natural -> VecaEvent -> (State Natural, VecaEvent)
s1 -| e = (State s1,e)

infix 1 |-> --
(|->) :: (State Natural, VecaEvent) -> Natural -> VecaTransition Natural
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
(◊) :: String -> DSLOperation -> JoinPoint
"self" ◊ o = JoinPoint Self $ op o
n ◊ o = JoinPoint (Name n) $ op o

self :: String
self = "self"

message :: String -> String -> Message
message m t = Message (Name m) (MessageType t)

operation :: String -> [Message] -> DSLOperation
operation s (m1:m2:ms) = DSLOperation (Operation $ Name s) m1 (Just m2)
operation s [m1]    = DSLOperation (Operation $ Name s) m1 Nothing
operation s _          = DSLOperation (Operation $ Name s) (message "" "") Nothing

tau :: VecaEvent
tau = CTau

receive :: DSLOperation -> VecaEvent
receive o = CReceive $ op o

reply :: DSLOperation -> VecaEvent
reply o = CReply $ op o

invoke :: DSLOperation -> VecaEvent
invoke o = CInvoke $ op o

result :: DSLOperation -> VecaEvent
result o = CResult $ op o

oreceive :: Operation -> VecaEvent
oreceive = CReceive

oreply :: Operation -> VecaEvent
oreply = CReply

oinvoke :: Operation -> VecaEvent
oinvoke = CInvoke

oresult :: Operation -> VecaEvent
oresult = CResult

provided :: [DSLOperation] -> [DSLOperation] -> Signature
provided os1 os2 =
  Signature {providedOperations = fmap op os1
            ,requiredOperations = fmap op os2
            ,input = fromList [(op o,inmsg o)|o <- os]
            ,output = fromList [(op o,outmsg o)|o <- os]}
  where os = os1 <> os2

required :: [DSLOperation] -> [DSLOperation]
required = id

behaviour :: Signature -> Natural -> [Natural] -> [VecaTransition Natural] -> VecaLTS Natural
behaviour sig s0 fs ts =
  LabelledTransitionSystem (alphabetForSignature sig)
                           ss
                           (State s0)
                           (fmap State fs)
                           ts
  where ss = fmap source ts <> fmap target ts
        alphabetForSignature
          :: Signature -> [VecaEvent]
        alphabetForSignature s =
          concat [
                  -- for each 2-way required operation o, result o
                  [oresult o|o <- requiredOperations s,isJust (output s ! o)]
                 ,
                  -- for each 2-way provided operation o, reply o
                  [oreply o|o <- providedOperations s,isJust (output s ! o)]
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

check :: (DSLOperation -> VecaEvent) -> DSLOperation -> [Natural] -> (DSLOperation -> VecaEvent) -> DSLOperation -> TimeConstraint
check f1 e1 r f2 e2 = TimeConstraint (f1 e1) (f2 e2) (minimum r) (maximum r)

basiccomponent :: String
               -> Signature
               -> VecaLTS Natural
               -> [TimeConstraint]
               -> Component Natural
basiccomponent = BasicComponent

subcomponents
  :: [(String,Component Natural)] -> Map Name (Component Natural)
subcomponents l = fromList [(Name s,c) | (s,c) <- l]

compositecomponent :: String
                   -> Signature
                   -> Map Name (Component Natural)
                   -> [Binding]
                   -> [Binding]
                   -> Component Natural
compositecomponent = CompositeComponent

internalbindings :: [Binding] -> [Binding]
internalbindings = id

externalbindings :: [Binding] -> [Binding]
externalbindings = id
