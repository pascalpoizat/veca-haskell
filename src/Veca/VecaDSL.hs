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
  ,subcomponents
  ,internalbindings
  ,externalbindings
  ,basiccomponent
  ,compositecomponent)
where

import           Data.Map                        (fromList, (!))
import           Data.Maybe                      (isJust)
import           Data.Monoid                     ((<>))
import           Models.Events                   (CIOEvent (..))
import           Models.LabelledTransitionSystem (LabelledTransitionSystem (..),
                                                  State (..), Transition (..))
import           Models.Name                     (Name (..))
import           Numeric.Natural                 as N (Natural)
import           Veca.Veca

data DSLOperation =
  DSLOperation {op      :: Operation
                ,inmsg  :: Message
                ,outmsg :: Maybe Message}

infix 2 -| --
(-|) :: String -> VEvent -> (VState, VEvent)
s1 -| e = (State s1,e)

infix 1 |-> --
(|->) :: (VState, VEvent) -> String -> VTransition
(s1,e) |-> s2 = Transition s1 e (State s2)

infix 1 -: --
(-:) :: String -> Component -> (String,Component)
n -: c = (n,c)

infix 2 <--> --
(<-->) :: JoinPoint -> JoinPoint -> Binding
j1 <--> j2 = ExternalBinding j1 j2

infix 2 >--< --
(>--<) :: JoinPoint -> JoinPoint -> Binding
j1 >--< j2 = InternalBinding j1 j2

infix 3 ◊ --
(◊) :: String -> DSLOperation -> JoinPoint
"self" ◊ o = JoinPoint self $ op o
n ◊ o = JoinPoint (Name [n]) $ op o

message :: String -> String -> Message
message m t = Message (Name [m]) (MessageType t)

operation :: String -> [Message] -> DSLOperation
operation s (m1:m2:ms) = DSLOperation (Operation $ Name [s]) m1 (Just m2)
operation s [m1]    = DSLOperation (Operation $ Name [s]) m1 Nothing
operation s _          = DSLOperation (Operation $ Name [s]) (message "" "") Nothing

tau :: VEvent
tau = CTau

receive :: DSLOperation -> VEvent
receive o = CReceive $ op o

reply :: DSLOperation -> VEvent
reply o = CReply $ op o

invoke :: DSLOperation -> VEvent
invoke o = CInvoke $ op o

result :: DSLOperation -> VEvent
result o = CResult $ op o

oreceive :: Operation -> VEvent
oreceive = CReceive

oreply :: Operation -> VEvent
oreply = CReply

oinvoke :: Operation -> VEvent
oinvoke = CInvoke

oresult :: Operation -> VEvent
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

behaviour :: Signature -> String -> [String] -> [VTransition] -> VLTS
behaviour sig s0 fs ts =
  LabelledTransitionSystem (alphabetForSignature sig)
                           ss
                           (State s0)
                           (fmap State fs)
                           ts
  where ss = fmap source ts <> fmap target ts
        alphabetForSignature
          :: Signature -> [VEvent]
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

check :: (DSLOperation -> VEvent) -> DSLOperation -> [Natural] -> (DSLOperation -> VEvent) -> DSLOperation -> TimeConstraint
check f1 e1 r f2 e2 = TimeConstraint (f1 e1) (f2 e2) (minimum r) (maximum r)

basiccomponent :: Name
               -> Signature
               -> VLTS
               -> [TimeConstraint]
               -> Component
basiccomponent = BasicComponent

subcomponents :: [(String,Component)] -> [(Name,Component)]
subcomponents l = [(Name [n], c) | (n,c) <- l]

compositecomponent :: Name
                   -> Signature
                   -> [(Name,Component)]
                   -> [Binding]
                   -> [Binding]
                   -> Component
compositecomponent = CompositeComponent

internalbindings :: [Binding] -> [Binding]
internalbindings = id

externalbindings :: [Binding] -> [Binding]
externalbindings = id
