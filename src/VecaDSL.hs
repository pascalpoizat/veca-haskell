-----------------------------------------------------------------------------
-- |
-- Module      :  VecaDSL
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- DSL for Veca.
-----------------------------------------------------------------------------

module VecaDSL ((-|)
               ,(|->)
               ,(-:)
               ,(◊)
               ,(<-->)
               ,(>--<)
               ,message
               ,VecaDSL.operation
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

import           Data.Map                 as M (Map, fromList, (!))
import           Data.Maybe               as X (isJust)
import           Data.Monoid              as DM (mappend)
import           Data.Set                 as S (Set, fromList, singleton,
                                                toList, unions)
import           LabelledTransitionSystem
import           Numeric.Natural          as N (Natural)
import           Veca

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
  Signature {providedOperations = S.fromList $ fmap op os1
            ,requiredOperations = S.fromList $ fmap op os2
            ,input = M.fromList [(op o,inmsg o)|o <- os]
            ,output = M.fromList [(op o,outmsg o)|o <- os]}
  where os = os1 `DM.mappend` os2

required :: [DSL_Operation] -> [DSL_Operation]
required = id

behaviour :: Signature -> Natural -> [Natural] -> [Transition BehaviorEvent Natural] -> Behavior Natural
behaviour sig s0 fs ts =
  LabelledTransitionSystem (alphabetForSignature sig)
                           ss
                           (State s0)
                           (S.fromList (fmap State fs))
                           (S.fromList ts)
  where ss = S.fromList $ (fmap source ts) `DM.mappend` (fmap target ts)
        alphabetForSignature
          :: Signature -> Set BehaviorEvent
        alphabetForSignature s =
          S.unions [
                    -- for each 2-way required operation o, result o
                    S.fromList
                      [oresult o
                      |o <- S.toList $ requiredOperations s
                      ,isJust ((output s) M.! o)]
                   ,
                    -- for each 2-way provided operation o, reply o
                    S.fromList
                      [oreply o
                      |o <- S.toList $ providedOperations s
                      ,isJust ((output s) M.! o)]
                   ,
                    -- for each required operation o, invoke o
                    S.fromList [oinvoke o|o <- S.toList $ requiredOperations s]
                   ,
                    -- for each provided operation o, receive o
                    S.fromList [oreceive o|o <- S.toList $ providedOperations s]
                   ,
                    -- tau
                    S.singleton CTau]

constraints :: [TimeConstraint] -> Set TimeConstraint
constraints l = S.fromList l

check :: (DSL_Operation -> BehaviorEvent) -> DSL_Operation -> [Natural] -> (DSL_Operation -> BehaviorEvent) -> DSL_Operation -> TimeConstraint
check f1 e1 r f2 e2 = TimeConstraint (f1 e1) (f2 e2) (minimum r) (maximum r)

basiccomponent :: Signature -> Behavior Natural -> Set TimeConstraint -> Component Natural
basiccomponent s b c = BasicComponent s b c

subcomponents :: [(String,Component Natural)] -> Map Name (Component Natural)
subcomponents l = M.fromList [(Name s,c) | (s,c) <- l]

compositecomponent :: Signature -> Map Name (Component Natural) -> Set Binding -> Set Binding -> Component Natural
compositecomponent s cs ibs ebs = CompositeComponent s cs ibs ebs

internalbindings :: [Binding] -> Set Binding
internalbindings l =  S.fromList l

externalbindings :: [Binding] -> Set Binding
externalbindings l = S.fromList l
