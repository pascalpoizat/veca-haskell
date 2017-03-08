-----------------------------------------------------------------------------
-- |
-- Module      :  Models.LabelledTransitionSystem
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A type for Labelled Transition System (LTS).
-----------------------------------------------------------------------------

module Models.LabelledTransitionSystem (
    -- * constructors
    State(..)
  , Transition(..)
  , LabelledTransitionSystem(..)
  , LTS
  , IOEvent(..)
  , IOLTS
  , CIOEvent(..)
  , CIOLTS
  , Path(..)
  , ComputationTree
    -- * validity checking
  , isValidLTS
    -- * reachability
  , successors
  , predecessors
  , xreachables
  , reachables
  , coreachables
    -- * paths
  , paths
  , pathsFrom
  , treePaths
    -- * properties
  , hasLoop
  , isSelfReachable
    -- * model to model transformations
  , toComputationTree
  , Models.LabelledTransitionSystem.toDot)
where

import           Data.GraphViz        (DotGraph, graphElemsToDot,
                                       nonClusteredParams)
import           Data.Monoid          (Any (..), (<>))
import           Helpers              (allIn, fixpoint')
import           Models.Complementary
import           Trees.Tree

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
  LabelledTransitionSystem {alphabet     :: [a]              -- ^ alphabet
                           ,states       :: [State b]        -- ^ set of states
                           ,initialState :: State b          -- ^ initial state
                           ,finalStates  :: [State b]        -- ^ set of final states
                           ,transitions  :: [Transition a b] -- ^ set of transitions
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
  deriving (Eq,Ord)

-- |Instance of Show for CIOEvent.
instance Show a =>
         Show (IOEvent a) where
  show Tau         = "tau"
  show (Receive a) = "receive " ++ (show a)
  show (Send a)    = "send " ++ (show a)

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
  deriving (Eq,Ord)

-- |Instance of Show for CIOEvent.
instance Show a =>
         Show (CIOEvent a) where
  show CTau         = "tau"
  show (CReceive a) = "receive " ++ (show a)
  show (CReply a)   = "reply " ++ (show a)
  show (CInvoke a)  = "invoke " ++ (show a)
  show (CResult a)  = "result " ++ (show a)

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
isValidLTS :: (Ord a
              ,Ord b)
           => LTS a b -> Bool
isValidLTS (LabelledTransitionSystem as ss s0 fs ts)
  | null as = False
  | null ss = False
  | not (s0 `elem` ss) = False
  | not $ fs `allIn` ss = False
  | not $ (fmap source ts) `allIn` ss = False
  | not $ (fmap label ts) `allIn` as = False
  | not $ (fmap target ts) `allIn` ss = False
  | otherwise = True

-- |Check if there are loops in a 'LabelledTransitionSystem'
hasLoop :: (Ord b)
        => LabelledTransitionSystem a b -> Bool
hasLoop (LabelledTransitionSystem as ss s0 sfs ts) =
  getAny $ foldMap (Any . isSelfReachable ts) ss

-- |Check if a 'State' is reachable from itself.
isSelfReachable
  :: (Ord b)
  => [Transition a b] -> State b -> Bool
isSelfReachable ts s = s `elem` reachables ts s

-- |Get the 'State's reachable from a 'State' in one transition.
successors
  :: (Ord b)
  => [Transition a b] -> State b -> [State b]
successors ts s = fmap target $ filter ((== s) . source) ts

-- |Get the 'State's co-reachable from a 'State' in one transition.
predecessors
  :: (Ord b)
  => [Transition a b] -> State b -> [State b]
predecessors ts s = fmap source $ filter ((== s) . target) ts

-- |Get all 'State's f-reachable from a 'State', where f is a step function.
xreachables :: (Ord b)
            => ([Transition a b] -> State b -> [State b])
            -> [Transition a b]
            -> State b
            -> [State b]
xreachables f ts s = fixpoint' (step f ts) $ f ts s
  where step :: (Ord b)
             => ([Transition a b] -> State b -> [State b])
             -> [Transition a b]
             -> [State b]
             -> [State b]
        step f' ts' ss = ss <> foldMap (f' ts') ss

-- |Get all 'State's reachable from a 'State'.
reachables
  :: (Ord b)
  => [Transition a b] -> State b -> [State b]
reachables = xreachables successors

-- |Get all 'State's co-reachable from a 'State'.
coreachables
  :: (Ord b)
  => [Transition a b] -> State b -> [State b]
coreachables = xreachables predecessors

-- |A path is a list triples (si,ai,s'i).
--
-- We do not verify the property that for each i we have si+1=s'i.
newtype Path a b = Path [(State b,a,State b)]
  deriving (Eq,Ord,Show)

-- |Monoid instance for 'Path'.
instance Monoid (Path a b) where
  mempty = Path []
  mappend (Path xs) (Path xs') = Path (xs <> xs')

-- |A computation tree for LTS.
type ComputationTree a b = Tree (State b) (State b) a

-- |Get all paths (from the initial state).
--
-- Can be infinite.
paths
  :: (Ord a, Ord b)
  => LabelledTransitionSystem a b -> [Path a b]
paths l = pathsFrom (initialState l) l

-- |Get paths from some 'State'.
--
-- Can be infinite.
pathsFrom
  :: (Ord a, Ord b)
  => State b -> LabelledTransitionSystem a b -> [Path a b]
pathsFrom s l = treePaths $ toComputationTree s l

-- |Get paths in a computation tree.
--
-- Can be infinite.
treePaths
  :: (Ord a, Ord b) => ComputationTree a b -> [Path a b]
treePaths = f mempty
  where f p (Leaf _)    = [p]
        f p (Node s ts) = p : foldMap (g p s) ts
        g p s (a,t'@(Leaf s'))     = f (p <> (Path [(s,a,s')])) t'
        g p s (a,t'@(Node s' ts')) = f (p <> (Path [(s,a,s')])) t'

-- |Build a computation tree from an LTS (starting with a distinct state).
--
-- Can be infinite.
toComputationTree :: (Eq b)
                  => State b
                  -> LabelledTransitionSystem a b
                  -> ComputationTree a b
toComputationTree s l@(LabelledTransitionSystem _ _ _ _ ts)
  | null ts' = Leaf s
  | otherwise = Node s $ foldMap f ts'
  where ts' = filter ((== s) . source) ts
        f (Transition _ x s2) = [(x,toComputationTree s2 l)]

-- |Transformation from 'LabelledTransitionSystem' to dot.
toDot
  :: (Ord a
     ,Ord b)
  => LabelledTransitionSystem a b -> DotGraph (State b)
toDot (LabelledTransitionSystem _ ss _ _ ts) =
  graphElemsToDot nonClusteredParams
                  (fmap stateToDotState ss)
                  (fmap transitionToDotEdge ts)

-- |Transformation from 'State' to dot state.
--
-- Helper for 'toDot', the 'LabelledTransitionSystem' to dot transformation.
stateToDotState :: State a -> (State a,State a)
stateToDotState s = (s,s)

-- |Transformation from 'Transition' to dot edge.
--
-- Helper for 'toDot', the 'LabelledTransitionSystem' to dot transformation.
transitionToDotEdge
  :: Transition a b -> (State b,State b,a)
transitionToDotEdge t = (source t,target t,label t)
