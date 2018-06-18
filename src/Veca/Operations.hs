{-|
Module      : Veca.Operations
Description : Functions for VECA
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Veca.Operations (
    -- * model transformation
    cToCTree
  , cToTA
  , cTreeToTAList
  , fLift
  , indexBy
  , genClock
    -- * other
  , operations
  , isCSource
  , isCTarget
  , isCPath)
where

import           Data.Aeson
import           Data.Bifunctor                  (second)
import           Data.Hashable                   (Hashable, hash, hashWithSalt)
import           Data.Map                        as M (Map, keysSet, member,
                                                       (!))
import           Data.Monoid                     (All (..), Any (..), (<>))
import           Data.Set                        as S (fromList)
import           GHC.Generics                    (Generic)
import           Models.Events                   (CIOEvent (..))
import           Models.LabelledTransitionSystem (LabelledTransitionSystem (..),
                                                  Path (..), State (..),
                                                  Transition (Transition, label, source),
                                                  end, hasLoop, isValidLTS,
                                                  paths', start)
import           Models.Name                     (Name (..), isValidName)
import           Models.Named                    (Named (..), suffixBy)
import           Models.TimedAutomaton           as TA (Clock (..),
                                                        ClockConstraint (..),
                                                        ClockOperator (GE, LE),
                                                        ClockReset (..),
                                                        Edge (Edge),
                                                        Location (..),
                                                        TimedAutomaton (..),
                                                        ToXta, asXta, relabel)
import           Numeric.Natural
import           Transformations.Substitution    (Substitution, apply, empty,
                                                  freevariables)
import           Trees.Tree
import           Trees.Trifunctor                (first)
import           Veca.Model

{-|
Get the operations of a component.

There may be duplicates in the returned list if the signature is not valid.
-}
operations :: Component -> [Operation]
operations c = pos <> ros
  where
    pos = providedOperations sig
    ros = requiredOperations sig
    sig = signature c

-- |Check if a transition is a possible source for a time constraint.
isCSource :: TimeConstraint -> VTransition -> Bool
isCSource k t = label t == startEvent k

-- |Check if a transition is a possible source for a time constraint (Maybe version).
isCSource' :: TimeConstraint -> Maybe VTransition -> Bool
isCSource' _ Nothing  = False
isCSource' k (Just t) = isCSource k t

-- |Check if a transition is a possible target for a time constraint.
isCTarget :: TimeConstraint -> VTransition -> Bool
isCTarget k t = label t == stopEvent k

-- |Check if a transition is a possible target for a time constraint (Maybe version).
isCTarget' :: TimeConstraint -> Maybe VTransition -> Bool
isCTarget' _ Nothing  = False
isCTarget' k (Just t) = isCTarget k t

-- |Check if a state is possibly concerned by a time constraint
-- a state s is concerned by a time constraint k wrt a behavior b
-- iff there is a path p = t1 t2 ... tn-1 tn in b such that:
-- - p is a path for k, and
-- - s is the source state of a transition in t2 ... tn-1 tn
isCState :: VLTS -> TimeConstraint -> VState -> Bool
isCState b k s = getAny $ foldMap (Any . isCPathForState k s) (paths' b)
  where
    isCPathForState _ _ (Path []) = False
    isCPathForState k' s' p@(Path (_:ts)) =
      isCPath k' p && getAny (foldMap (Any . (== s') . source) ts)

-- |Check if a path is possibly concerned by a time constraint.
-- That is, if the path:
-- - starts with a transition that is a possible source for the time constraint, and
-- - ends with a transition that is a possible target for the time constraint.
isCPath :: TimeConstraint -> VPath -> Bool
isCPath k p = isCSource' k (start p) && isCTarget' k (end p)

-- |Transform an architecture (given as a component instance) into a timed automaton tree
-- TODO: DEAD CODE
-- cToTATree :: ComponentInstance -> VTATree
-- cToTATree = cTreeToTATree . cToCTree

-- |Transform an architecture (given as a component instance) into a component tree
cToCTree :: ComponentInstance -> VCTree
cToCTree c@(ComponentInstance _ BasicComponent {}) = Leaf c
cToCTree c@(ComponentInstance _ (CompositeComponent _ _ cs _ _)) = Node c cs'
  where
    cs' = indexInstance <$> cs
    indexInstance ci = (instanceId ci, cToCTree ci)

-- |Transform a component tree into a timed automaton tree
-- TODO: DEAD CODE
-- cTreeToTATree :: VCTree -> VTATree
-- cTreeToTATree = mapleaves cToTA

-- |Transform a component into a timed automaton
cToTA :: ComponentInstance -> VTA
cToTA (ComponentInstance i (BasicComponent _ _ b cts)) =
  TimedAutomaton i ls l0 cs as es is
  where
    ls = toLocation <$> states b
    l0 = toLocation (initialState b)
    cs = genClock <$> cts
    as = alphabet b ++ [CTau]
    es =
      (genEdge cts <$> transitions b) ++
      (genLoopOn . toLocation <$> finalStates b)
    is = genInvariant cts b <$> states b
cToTA (ComponentInstance _ CompositeComponent {}) = undefined -- TODO: define using cToTA and flatten

-- |Transform a state into a location
toLocation :: VState -> VLocation
toLocation (State s) = Location s

-- |Generate a clock for a time constraint
genClock :: TimeConstraint -> Clock
genClock =
  Clock .
  (\h ->
     if h >= 0
       then show h
       else '_' : show (-h)) .
  hash

-- |Generate an edge for a transition
genEdge :: [TimeConstraint] -> VTransition -> VEdge
genEdge ks t@(Transition s1 a s2) = Edge s1' a g r s2'
  where
    s1' = toLocation s1
    g = [genCBegin k | k <- filter (`isCTarget` t) ks]
    r = [genReset k | k <- filter (`isCSource` t) ks]
    s2' = toLocation s2

-- |Generate a looping tau edge for a state
genLoopOn :: VLocation -> VEdge
genLoopOn l = Edge l CTau [] [] l

-- |Generate a reset for the clock of a time constraint
genReset :: TimeConstraint -> ClockReset
genReset = ClockReset . genClock

-- |Generate the invariant for a state
genInvariant ::
     [TimeConstraint] -> VLTS -> VState -> (VLocation, [ClockConstraint])
genInvariant ks b s = (toLocation s, [genCEnd k | k <- ks, isCState b k s])

-- |Generate a clock constraint "clock GE beginTime" from a time constraint
genCBegin :: TimeConstraint -> ClockConstraint
genCBegin k = ClockConstraint (genClock k) GE (beginTime k)

-- |Generate a clock constraint "clock LE endTime" from a time constraint
genCEnd :: TimeConstraint -> ClockConstraint
genCEnd k = ClockConstraint (genClock k) LE (endTime k)

{-|
Flatten a VecaTATree into a list of TimedAutomata.
-}
cTreeToTAList :: VCTree -> [VTA]
cTreeToTAList = cTreeToTAList' mempty empty

{-|
Helper to flatten a VecaTATree into a list of TimedAutomata.
-}
cTreeToTAList' :: VName -> VOSubstitution -> VCTree -> [VTA]
-- for a leaf:
-- transform the component into a timed automaton
-- apply the substitution lifted from operations to events to this timed automaton
-- prefix its id by p
cTreeToTAList' p sub (Leaf ci) = [prefixBy p . relabel sub' $ ta]
  where
    ta = cToTA ci
    sub' = liftOSubToESub sub
-- for a node:
-- build a new substitution for each subcomponent ti and recurse
-- this new substitution is as follows:
-- - for each op of it that is already covered by sigma, keep the corresponding substitution
-- - for each op not covered by sigma this is in an internal binding, prefix by p and the binding id
-- - for each op not covered by sigma that is not connected by a binding, prefix by p
cTreeToTAList' p sigma (Node (ComponentInstance iid c@CompositeComponent {}) xs) =
  concat (iter <$> subtrees)
  where
    subtrees = fmap snd xs
    p' = p <> iid
    iter ti = cTreeToTAList' p' sigma' ti
      where
        c = component ti
        component (Leaf c)   = c
        component (Node c _) = c
        ops = operations . componentType $ c
        sigma' = sigmaE <> sigmaI <> sigmaN
          where
            sigmaE = []
            sigmaI = []
            sigmaN = []
    --sigma' = sigma <> (genSub <$> freeops)
    genSub o = (o, indexBy p' o)
    freeops =
      freevariables sigma $ (operation . from) <$> (inbinds c <> extbinds c)
cTreeToTAListt' _ _ (Node (ComponentInstance _ BasicComponent {}) _) = undefined

{-|
Get the component instance at the root of a tree.
-}
rootInstance :: VCTree -> ComponentInstance
rootInstance (Leaf c)   = c
rootInstance (Node c _) = c

{-|
Get the instance id at the root of a tree.
-}
rootId :: VCTree -> VName
rootId = instanceId . rootInstance

{-|
Index an operation by a name.
-}
indexBy :: VName -> Operation -> Operation
indexBy i (Operation n) = Operation (i <> n)

{-|
Map a function on the leaves of a tree.

Just a renaming for first in Trifunctor.
-}
mapleaves :: (a -> a') -> Tree a b c -> Tree a' b c
mapleaves = first

{-|
Lift a substitution over operations to a substitution over events.
-}
liftOSubToESub :: VOSubstitution -> VESubstitution
liftOSubToESub = foldMap (fLift [CReceive, CReply, CInvoke, CResult])

{-|
Lift a couple wrt. a collection of functions.

TODO: generalize to any Monoid.
-}
fLift :: Functor f => f (a -> b) -> (a, a) -> f (b, b)
fLift cs (a1, a2) = f a1 a2 <$> cs
  where
    f x y c = (c x, c y)
