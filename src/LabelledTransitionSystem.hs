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

module LabelledTransitionSystem (-- * constructors
                                  State(..)
                                , Transition(..)
                                , LTS(..)
                                , IOEvent(..)
                                , IOLTS
                                , CIOEvent(..)
                                , CIOLTS
                                -- * validity checking
                                , isValidLTS
                                -- * constructor helpers
                                , tau
                                , ctau
                                -- * model to model transformations
                                , LabelledTransitionSystem.toDot)
where

import           Complementary
import           Data.GraphViz as GV
import           Data.Set      as S (Set, isSubsetOf, map, member, null, toList)

-- |A state. This is the encapsulation of a String.
data State
  = State String
  deriving (Eq,Ord,Show)

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

-- |Complementary for a 'IOEvent'.
instance Complementary IOEvent where
  complementary Tau = Tau
  complementary (Receive a) = (Send a)
  complementary (Send a) = (Receive a)

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

-- |Complementary for a 'IOEvent'.
instance Complementary CIOEvent where
  complementary CTau = CTau
  complementary (CReceive a) = (CInvoke a)
  complementary (CReply a) = (CResult a)
  complementary (CInvoke a) = (CReceive a)
  complementary (CResult a) = (CReply a)

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
