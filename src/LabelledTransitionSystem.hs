-----------------------------------------------------------------------------
-- |
-- Module      :  LabelledTransitionSystem
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A type for Labelled Transition System (LTS).
-----------------------------------------------------------------------------

module LabelledTransitionSystem (-- * basic types
                                  State
                                -- * constructors
                                , Transition(..)
                                , LTS(..)
                                -- * instantiated types
                                , IOEvent(..)
                                , IOLTS
                                , CIOEvent(..)
                                , CIOLTS
                                -- * validity checking
                                , isValidLTS
                                -- * helpers to construct values
                                , tau
                                , ctau
                                -- * predicates
                                , complementary
                                , ccomplementary
                                -- * model to text transformations
                                -- * model to model transformations
                                , LabelledTransitionSystem.toDot)
where

import           Data.GraphViz as GV
import           Data.Set      as S (Set, isSubsetOf, map, member, null, toList)

-- |A state. This is simply a String.
type State = String

-- |A transition with a label of type a.
data Transition a =
  Transition {source :: State -- ^ source state of the 'Transition'
             ,label  :: a     -- ^ label of the 'Transition'
             ,target :: State -- ^ target state of the 'Transition'
             }
  deriving (Show,Eq,Ord)

-- |A Labelled Transition System ('LTS') with labels of type a.
data LTS a =
  LTS {alphabet     :: Set a              -- ^ alphabet of the 'LTS'
      ,states       :: Set State          -- ^ set of states of the 'LTS'
      ,initialState :: State              -- ^ initial state of the 'LTS'
      ,finalStates  :: Set State          -- ^ set of final states of the 'LTS'
      ,transitions  :: Set (Transition a) -- ^ set of transitions of the 'LTS'
      }
  deriving (Show)

-- |Input-Output Events (IOEvents).
-- Used as labels in 'IOLTS's.
data IOEvent a
  = Tau       -- ^ internal action (non-observable)
  | Receive a -- ^ reception of something
  | Send a    -- ^ sending of something
  deriving (Show,Eq,Ord)

-- |An Input-Output LTS (IOLTS).
-- This is an 'LTS' where labels are of type 'IOEvent'.
type IOLTS a = LTS (IOEvent a)

-- |Communication-Input-Output Events (CIOEvents).
-- Used as labels in 'CIOLTS's.
data CIOEvent a
  = CTau       -- ^ internal action (non-observable)
  | CReceive a -- ^ reception of a call
  | CReply a   -- ^ reply to a call
  | CInvoke a  -- ^ passing a call (= invocation)
  | CResult a  -- ^ getting the result of a call
  deriving (Show,Eq,Ord)

-- |Communication-Input-Output LTS (CIOLTS).
-- This is an 'LTS' where labels are of type 'CIOEvent'.
type CIOLTS a = LTS (CIOEvent a)

-- |Check the validity of an 'LTS'.
-- An 'LTS' is valid iff:
--
-- - the alphabet is not empty
-- - the set of states is not empty
-- - the initial state is in the set of states
-- - each final state is in the set of states
-- - the source state of each transition is in the set of states
-- - the label of each transition is in the alphabet
-- - the target state of each transition is in the set of states
isValidLTS :: (Ord a) => LTS a -> Bool
isValidLTS (LTS as ss s0 fs ts) =
  not (S.null as) &&
  not (S.null ss) &&
  s0 `member` ss &&
  fs `isSubsetOf` ss &&
  S.map source ts `isSubsetOf` ss &&
  S.map target ts `isSubsetOf` ss && S.map label ts `isSubsetOf` as

-- |Create a 'Transition' with a 'IOEvent' 'Tau' label.
--
-- Use @tau s1 s2@
-- instead of @Transition s1 Tau s2@
tau :: State -> State -> Transition (IOEvent a)
tau s1 s2 = Transition s1 Tau s2

-- |Create a 'Transition' with a 'CIOEvent' 'CTau' label.
--
-- Use @ctau s1 s2@
-- instead of @Transition s1 CTau s2@
ctau :: State -> State -> Transition (CIOEvent a)
ctau s1 s2 = Transition s1 CTau s2

-- |Check if two 'IOEvent's are complementary.
--
-- @
-- (Receive a) (Send b) : True if a == b
-- (Send a) (Receive b) : True if a == b
-- otherwise            : False
-- @
complementary :: Eq a => IOEvent a -> IOEvent a -> Bool
complementary (Receive a) (Send b) = a == b
complementary (Send a) (Receive b) = a == b
complementary _ _                  = False

-- |Check if two 'CIOEvent's are complementary.
--
-- @
-- (CReceive a) (CInvoke b) : True if a == b
-- (CInvoke a) (CReceive b) : True if a == b
-- (CReply a) (CResult b)   : True if a == b
-- (CResult a) (CReply b)   : True if a == b
-- otherwise                : False
-- @
ccomplementary :: Eq a => CIOEvent a -> CIOEvent a -> Bool
ccomplementary (CReceive a) (CInvoke b) = a == b
ccomplementary (CInvoke a) (CReceive b) = a == b
ccomplementary (CReply a) (CResult b)   = a == b
ccomplementary (CResult a) (CReply b)   = a == b
ccomplementary _ _                      = False

-- |Transformation from 'LTS' to dot.
toDot :: (Ord a) => LTS a -> DotGraph State
toDot (LTS _ ss _ _ ts) =
  graphElemsToDot nonClusteredParams
                  (toList $ S.map stateToDotState ss)
                  (toList $ S.map transitionToDotEdge ts)

-- |Transformation from 'State' to dot state.
--
-- Helper for 'toDot', the 'LTS' to dot transformation.
stateToDotState :: State -> (State, State)
stateToDotState s = (s,s)

-- |Transformation from 'Transition' to dot edge.
--
-- Helper for 'toDot', the 'LTS' to dot transformation.
transitionToDotEdge
  :: Transition a -> (State,State,a)
transitionToDotEdge t = (source t, target t, label t)


