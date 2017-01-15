-- module for Trees
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
--
module LabelledTransitionSystem ( LTS(..)
                                , Transition(..)
                                , isValid
                                , LabelledTransitionSystem.toDot
                                , IOLTS(..)
                                , IOEvent(..)
                                , tauTransition
                                , complementary
                                , CIOLTS(..)
                                , CIOEvent(..)
                                , cTauTransition
                                , ccomplementary)
where

import           Data.GraphViz as GV
import           Data.Set      as S (Set, isSubsetOf, map, member, null, toList)

-- simple types
type State = String

-- Transition = (source state, label, target s)
data Transition a =
  Transition {source :: State
             ,label  :: a
             ,target :: State}
  deriving (Show,Eq,Ord)

-- LTS = (alphabet, States, initial state, final states, transitions)
data LTS a =
  LTS {alphabet     :: Set a
      ,states       :: Set State
      ,initialState :: State
      ,finalStates  :: Set State
      ,transitions  :: Set (Transition a)}
  deriving Show

-- IOLTS = an LTS where each element in the alphabet is either tau, receive x or send x
data IOEvent a
  = Tau
  | Receive a
  | Send a
  deriving (Show,Eq,Ord)

type IOLTS a = LTS (IOEvent a)

tauTransition :: State -> State -> Transition (IOEvent a)
tauTransition s1 s2 =  Transition s1 Tau s2

-- check if two io events are complementary
complementary :: Eq a => IOEvent a -> IOEvent a -> Bool
complementary Tau Tau              = True
complementary (Receive a) (Send b) = a == b
complementary (Send a) (Receive b) = a == b
complementary _ _                  = False

-- CIOLTS = an LTS where we separate invocation from reply and reception from getting a result
data CIOEvent a
  = CTau
  | CReceive a
  | CReply a
  | CInvoke a
  | CResult a
  deriving (Show,Eq,Ord)

type CIOLTS a = LTS (CIOEvent a)

cTauTransition :: State -> State -> Transition (CIOEvent a)
cTauTransition s1 s2 = Transition s1 CTau s2

-- check if two cio events are complementary
ccomplementary :: Eq a => CIOEvent a -> CIOEvent a -> Bool
ccomplementary CTau CTau                = True
ccomplementary (CReceive a) (CInvoke b) = a == b
ccomplementary (CInvoke a) (CReceive b) = a == b
ccomplementary (CReply a) (CResult b)   = a == b
ccomplementary (CResult a) (CReply b)   = a == b
ccomplementary _ _                      = False

-- check the validity of an LTS
isValid :: (Ord a) => LTS a -> Bool
isValid (LTS as ss s0 fs ts) =
  not (S.null as) && -- the alphabet is not empty
  not (S.null ss) && -- the set of states is not empty
  s0 `member` ss && -- the initial state is a state
  fs `isSubsetOf` ss && -- all final states are states
  S.map source ts `isSubsetOf` ss && -- transitions rely a (source) state
  S.map target ts `isSubsetOf` ss && -- to a (target) state
  S.map label ts `isSubsetOf` as -- wrt an element of the alphabet

-- graphviz representation
stateToDotState :: State -> (State, State)
stateToDotState s = (s,s)

transitionToDotEdge
  :: Transition a -> (State,State,a)
transitionToDotEdge t = (source t, target t, label t)

toDot :: (Ord a) => LTS a -> DotGraph State
toDot (LTS _ ss _ _ ts) =
  graphElemsToDot nonClusteredParams
                  (toList $ S.map stateToDotState ss)
                  (toList $ S.map transitionToDotEdge ts)
