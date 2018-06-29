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
import           Data.Map                        as M (Map, empty, keysSet,
                                                       member, (!))
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
import           Transformations.Substitution    (Substitution (..), apply,
                                                  freevariables, isBound)
import           Trees.Tree                      (Tree (..))
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
  TimedAutomaton i ls l0 cls uls cs vs as es is
  where
    ls = toLocation <$> states b
    l0 = toLocation (initialState b)
    cls = []
    uls = []
    cs = genClock <$> cts
    vs = empty -- TODO: update
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
cTreeToTAList = cTreeToTAList' (mempty :: VName) (mempty :: VOSubstitution)

{-|
Helper to flatten a VecaTATree into a list of TimedAutomata.
-}
cTreeToTAList' :: VName -> VOSubstitution -> VCTree -> [VTA]
-- for a leaf (contains a component instance of a basic component type):
-- - transform the component instance c in l into a timed automaton ta
-- - lift the substitution sub on operations to one on events, sub'
-- - apply sub' to ta
-- - prefix the name of ta by p
cTreeToTAList' p sub (Leaf c) = [prefixBy p . relabel sub' $ ta]
 where
  ta   = cToTA c
  sub' = liftOSubToESub sub
-- for a node (contains a component instance x of a composite component type ct):,
-- - define p' as p.x
-- - for each component instance c_i in ct (it requires using snd and value to get them):
--   (=iterate)
--   build a substitution sub'_i and recurse with cTreeToTAList p' sub'_i c_i where
--   for each op_i_j of c_i:
--   (=buildSub)
--   - if c_i.op_i_j is in an external binding k of c,
--     then if op_i_j is in sub, then (op_i_j, sub(op_i_j)) in sub_'i
--     else (op_i_j, p'.k.op_i_j) in sub'_i
--   - if c_i.op_i_j is in an internal binding k of c, then (op_i_j, p'.k.op_i_j) in sub'_i
--   - else (op_i_j is unbound), (op_i_j, p'.c_i.op_i_j) in sub'_i
-- note: an operation cannot be in more than on binding (this is checked using the VECA IDE).
cTreeToTAList' p sub (Node c xs) = foldMap iterate (snd <$> xs)
 where
  x  = instanceId c
  ct = componentType c
  p' = p <> x
  iterate sti = cTreeToTAList' p' (sub' . value $ sti) sti
   where
    sub' ci = foldMap (buildSub ci) (operations . componentType $ ci)
    buildSub ci' opij = case findEB ct ci' opij of
      Just k -> if isBound sub opij
        then opij |-> apply sub opij
        else opij |-> indexBy (p' <> k) opij
      _ -> case findIB ct ci' opij of
        Just k -> opij |-> indexBy (p' <> k) opij
        _      -> opij |-> indexBy (p' <> instanceId ci') opij

infix 3 |-> --
(|->) :: Operation -> Operation -> VOSubstitution
o |-> o' = [(o, o')]

findB :: [Binding] -> ComponentInstance -> Operation -> Maybe VName
findB bs ci o =
  let
    candidates = filter cond bs
    cond b = ok ci o (from b) || ok ci o (to b)
    ok ci o jp = (jpname jp == instanceId ci) && (jpoperation jp == o)
  in
    case candidates of
      [] -> Nothing
      _  -> Just . bindingId . head $ candidates

findEB :: Component -> ComponentInstance -> Operation -> Maybe VName
findEB (CompositeComponent _ _ _ _ ebs) ci o = findB ebs ci o
findEB BasicComponent{} _ _                  = Nothing

findIB :: Component -> ComponentInstance -> Operation -> Maybe VName
findIB (CompositeComponent _ _ _ ibs _) ci o = findB ibs ci o
findIB BasicComponent{} _ _                  = Nothing

{-|
Helper to get the root value in a tree (provided leaves and nodes have the same kind of value).
-}
value :: Tree a a c -> a
value (Leaf x)   = x
value (Node x _) = x

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
-}
fLift :: Functor f => f (a -> b) -> (a, a) -> f (b, b)
fLift cs (a1, a2) = f a1 a2 <$> cs
  where
    f x y c = (c x, c y)
