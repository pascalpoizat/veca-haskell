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
                                , LabelledTransitionSystem(..)
                                , LTS
                                , IOEvent(..)
                                , IOLTS
                                , CIOEvent(..)
                                , CIOLTS
                                -- * validity checking
                                , isValidLTS
                                -- * reachability
                                , successorStates
                                , reachableStates
                                -- * properties
                                , hasLoop
                                , isSelfReachable
                                -- * model to model transformations
                                , LabelledTransitionSystem.toDot)
where

import           Complementary
import           Data.GraphViz as GV
import           Data.Set      as S  (Set, filter, foldr, isSubsetOf, map,
                                      member, null, toList, union)

import           Data.Monoid         (Any (..), (<>))

-- |A state.
data State a
  = State a
  deriving (Eq,Ord,Show)

-- |A transition with a label of type a.
data Transition a b =
  Transition {source :: State b -- ^ source state of the 'Transition'
             ,label  :: a       -- ^ label of the 'Transition'
             ,target :: State b -- ^ target state of the 'Transition'
             }
  deriving (Show,Eq,Ord)

-- |A Labelled Transition System ('LTS') with labels of type a.
data LabelledTransitionSystem a b =
  LabelledTransitionSystem {alphabet     :: Set a                -- ^ alphabet
                           ,states       :: Set (State b)        -- ^ set of states
                           ,initialState :: State b              -- ^ initial state
                           ,finalStates  :: Set (State b)        -- ^ set of final states
                           ,transitions  :: Set (Transition a b) -- ^ set of transitions
                           }
  deriving (Show)

-- |Alias for LTS.
type LTS = LabelledTransitionSystem

-- |Input-Output Events (IOEvents).
-- Used as labels in 'IOLTS's.
data IOEvent a
  = Tau       -- ^ internal action (non-observable)
  | Receive a -- ^ reception of something
  | Send a    -- ^ sending of something
  deriving (Show,Eq,Ord)

-- |Complementary for a 'IOEvent'.
instance Complementary (IOEvent a) where
  complementary Tau         = Tau
  complementary (Receive a) = Send a
  complementary (Send a)    = Receive a

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
instance Complementary (CIOEvent a) where
  complementary CTau         = CTau
  complementary (CReceive a) = CInvoke a
  complementary (CReply a)   = CResult a
  complementary (CInvoke a)  = CReceive a
  complementary (CResult a)  = CReply a

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
isValidLTS :: (Ord a, Ord b) =>LTS a b -> Bool
isValidLTS (LabelledTransitionSystem as ss s0 fs ts)
  | S.null as = False
  | S.null ss = False
  | not (s0 `member` ss) = False
  | not (fs `isSubsetOf` ss) = False
  | not (S.map source ts `isSubsetOf` ss) = False
  | not (S.map label ts `isSubsetOf` as) = False
  | not (S.map target ts `isSubsetOf` ss) = False
  | otherwise = True

-- |Check if there are loops in a 'LabelledTransitionSystem'
hasLoop :: (Ord b) => LabelledTransitionSystem a b -> Bool
hasLoop (LabelledTransitionSystem as ss s0 sfs ts) =
  getAny $ foldMap (Any . isSelfReachable ts) ss

-- |Check if a 'State' is reachable from itself.
isSelfReachable :: (Ord b) => Set (Transition a b) -> State b -> Bool
isSelfReachable ts s = s `S.member` reachableStates ts s

-- |Get the 'State's reachable from a 'State' in one transition.
successorStates :: (Ord b) => Set (Transition a b) -> State b -> Set (State b)
successorStates ts s = S.map target $ S.filter ((==s). source) ts

-- |Get all 'State's reachable from a 'State'.
reachableStates :: (Ord b) => Set (Transition a b) -> State b -> Set (State b)
reachableStates ts s  = fixpoint (step ts) $ successorStates ts s
  where
    step :: (Ord b) => Set (Transition a b) -> Set (State b) -> Set (State b)
    step tts ss = ss <> foldMap (successorStates tts) ss

-- |Fixpoint
fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x
  | x == x' = x
  | otherwise = fixpoint f x'
  where x' = f x

-- |Transformation from 'LTS' to dot.
toDot :: (Ord a, Ord b) => LTS a b -> DotGraph (State b)
toDot (LabelledTransitionSystem _ ss _ _ ts) =
  graphElemsToDot nonClusteredParams
                  (toList $ S.map stateToDotState ss)
                  (toList $ S.map transitionToDotEdge ts)

-- |Transformation from 'State' to dot state.
--
-- Helper for 'toDot', the 'LTS' to dot transformation.
stateToDotState :: State a -> (State a, State a)
stateToDotState s = (s,s)

-- |Transformation from 'Transition' to dot edge.
--
-- Helper for 'toDot', the 'LTS' to dot transformation.
transitionToDotEdge
  :: Transition a b -> (State b,State b,a)
transitionToDotEdge t = (source t, target t, label t)
