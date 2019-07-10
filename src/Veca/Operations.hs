{-|
Module      : Veca.Operations
Description : Functions for VECA
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Veca.Operations
  (
    -- * model transformation
    cToCTree
  , cToTA
  , cTreeToTAList
  , fLift
  , indexBy
    -- * other
  , operations
  )
where

import           Control.Monad.State as MS
import           Data.Aeson
import           Data.Bifunctor                 ( second )
import           Data.Hashable                  ( Hashable
                                                , hash
                                                , hashWithSalt
                                                )
import           Data.Map                      as M
                                                ( Map
                                                , empty
                                                , keysSet
                                                , member
                                                , (!)
                                                )
import           Data.Monoid                    ( All(..)
                                                , Any(..)
                                                , (<>)
                                                )
import           Data.Set                      as S
                                                ( fromList )
import           GHC.Generics                   ( Generic )
import           Models.Events                  ( CTIOEvent(..)
                                                , liftToCTIOEvent
                                                )
import           Models.LabelledTransitionSystem
                                                ( LabelledTransitionSystem(..)
                                                , Path(..)
                                                , State(..)
                                                , Transition
                                                  ( Transition
                                                  , label
                                                  , source
                                                  )
                                                , end
                                                , hasLoop
                                                , isValidLTS
                                                , paths'
                                                , start
                                                )
import           Models.Name                    ( Name(..)
                                                , isValidName
                                                )
import           Models.Named                   ( Named(..)
                                                , suffixBy
                                                )
import           Models.TimedAutomaton         as TA
                                                ( Clock(..)
                                                , ClockConstraint(..)
                                                , ClockOperator(GE, LE)
                                                , ClockReset(..)
                                                , Edge(Edge)
                                                , Location(..)
                                                , TimedAutomaton(..)
                                                , ToXta
                                                , asXta
                                                , addObservers
                                                , relabel
                                                )
import           Numeric.Natural
import           Transformations.Substitution   ( Substitution(..)
                                                , apply
                                                , freevariables
                                                , isBound
                                                )
import           Trees.Tree                     ( Tree(..) )
import           Trees.Trifunctor               ( first )
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

-- |Transform an architecture (given as a component instance) into a component tree
cToCTree :: ComponentInstance -> VCTree
cToCTree c@(ComponentInstance _ BasicComponent{}               ) = Leaf c
cToCTree c@(ComponentInstance _ (CompositeComponent _ _ cs _ _)) = Node c cs'
 where
  cs' = indexInstance <$> cs
  indexInstance ci = (instanceId ci, cToCTree ci)

data TABuildState = TABS {nextId:: Integer, ta:: VTA, beh:: VLTS}

cToTAAddFinals :: [VState] -> MS.State TABuildState ()
cToTAAddFinals [] = return ()
cToTAAddFinals (f:fs) = do
  TABS n ta beh <- get
  let newLoc = Location ("_" ++ show n)
  let edge1 = Edge (toLocation f) CTTau [] [] [] newLoc
  let edge2 = Edge newLoc CTTau [] [] [] (toLocation f)
  put $ TABS (n+1) (update ta [newLoc] [edge1,edge2]) beh
  cToTAAddFinals fs
  return ()
  where
    update (TimedAutomaton i ls l0 cls uls cs vs as es is) newL newE =
      TimedAutomaton i (newL++ls) l0 cls uls cs vs as (newE++es) is

-- |Transform a component into a timed automaton (state monadic helper)
cToTA' :: MS.State TABuildState ()
cToTA' = do
  currentState <- get
  -- add new states and loops for final states
  let fs = finalStates . beh $ currentState
  cToTA_addFinals fs
  return ()

-- |Transform a component into a timed automaton (calls the state monadic helper)
cToTA :: ComponentInstance -> VTA
cToTA (ComponentInstance i (BasicComponent _ _ b)) =
  let ta0 = TimedAutomaton i ls l0 cls uls cs vs as es is
  in
    ta . snd $ runState cToTA' (TABS 0 ta0 b)
    where
      ls = lsorig
      l0 = toLocation (initialState b)
      cls = []
      uls = lsorig
      cs = [Clock "0"]
      vs = empty
      as = events (alphabet b) ++ [CTTau]
      es = []
      is = []
      lsorig = toLocation <$> states b
      events []                   = []
      events (EventLabel e : ees) = liftToCTIOEvent e : events ees
      events (_            : ees) = events ees
cToTA (ComponentInstance _ CompositeComponent{}) = undefined -- TODO: define using cToTA and flatten

-- |Transform a state into a location
toLocation :: VState -> VLocation
toLocation (State s) = Location s

-- |Generates a new location from a state
toLocationNew :: [Int] -> VState -> ([Int], VLocation)
toLocationNew (id : ids) (State s) = (ids, Location $ s ++ show id)

-- |Generate a looping tau edge for a state
genFinalLoop :: [Int] -> VState -> ([Int], [VTEdge])
genFinalLoop ids s =
  (ids', [Edge l CTTau [] [] [] lnew, Edge lnew CTTau [] [] [] l])
 where
  l            = toLocation s
  (ids', lnew) = toLocationNew ids s

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
  let candidates = filter cond bs
      cond b = ok ci o (from b) || ok ci o (to b)
      ok ci o jp = (jpname jp == instanceId ci) && (jpoperation jp == o)
  in  case candidates of
        [] -> Nothing
        _  -> Just . bindingId . head $ candidates

findEB :: Component -> ComponentInstance -> Operation -> Maybe VName
findEB (CompositeComponent _ _ _ _ ebs) ci o = findB ebs ci o
findEB BasicComponent{}                 _  _ = Nothing

findIB :: Component -> ComponentInstance -> Operation -> Maybe VName
findIB (CompositeComponent _ _ _ ibs _) ci o = findB ibs ci o
findIB BasicComponent{}                 _  _ = Nothing

{-|
Helper to get the root value in a tree (provided leaves and nodes have the same kind of value).
-}
value :: Tree a a c -> a
value (Leaf x  ) = x
value (Node x _) = x

{-|
Get the component instance at the root of a tree.
-}
rootInstance :: VCTree -> ComponentInstance
rootInstance (Leaf c  ) = c
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
liftOSubToESub :: VOSubstitution -> VTESubstitution
liftOSubToESub = foldMap (fLift [CTReceive, CTReply, CTInvoke, CTResult])

{-|
Lift a couple wrt. a collection of functions.
-}
fLift :: Functor f => f (a -> b) -> (a, a) -> f (b, b)
fLift cs (a1, a2) = f a1 a2 <$> cs where f x y c = (c x, c y)
