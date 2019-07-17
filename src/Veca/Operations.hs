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

import           Relude.Extra.Tuple
import           Data.Maybe
import           Control.Monad                  ( when )
import           Control.Monad.State           as MS
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
import           Models.Communication
import           Models.LabelledTransitionSystem
                                                ( LabelledTransitionSystem(..)
                                                , Path(..)
                                                , State(..)
                                                , Transition
                                                  ( Transition
                                                  , label
                                                  , source
                                                  , target
                                                  )
                                                , end
                                                , outgoing
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

cToTAAddFinals' :: [VState] -> MS.State TABuildState ()
cToTAAddFinals' []       = return ()
cToTAAddFinals' (f : fs) = do
  TABS n ta beh <- get
  let newLoc = Location ("_" ++ show n)
  let edge1  = Edge (toLocation f) CTTau [] [] [] newLoc
  let edge2  = Edge newLoc CTTau [] [] [] (toLocation f)
  put $ TABS (n + 1) (update ta [newLoc] [edge1, edge2]) beh
  cToTAAddFinals' fs
  return ()
 where
  update (TimedAutomaton i ls l0 cls uls cs vs as es is) newL newE =
    TimedAutomaton i (newL ++ ls) l0 cls uls cs vs as (newE ++ es) is

cToTAAddFinals :: MS.State TABuildState ()
cToTAAddFinals = do
  currentState <- get
  let fs = finalStates . beh $ currentState
  cToTAAddFinals' fs
  return ()

cToTAAddTimeouts''' :: VLocation -> [VTransition] -> MS.State TABuildState ()
cToTAAddTimeouts''' _ []       = return ()
cToTAAddTimeouts''' l (t : ts) = do
  let ll = label t
  when (isEventLabel ll) $ do
    TABS n ta beh <- get
    let (EventLabel e) = label t
    let newEdge = Edge l (liftToCTIOEvent e) [] [] [] (toLocation $ target t)
    put $ TABS n (update ta [] [newEdge] []) beh
  cToTAAddTimeouts''' l ts
  return ()

cToTAAddTimeouts'' :: VState -> VTransition -> MS.State TABuildState ()
cToTAAddTimeouts'' s t = do
  TABS n ta beh <- get
  let ts = transitions beh
  let s' = target t
  let c  = Clock "0"
  let l  = label t
  when (isTimeoutLabel l) $ do
    let (TimeoutLabel x) = label t
    -- add new location and its invariant
    let newLoc           = Location ("_" ++ show n)
    let newInv           = (newLoc, [ClockConstraint c LE x])
    -- add edges
    let newEdge1 = Edge (toLocation s) CTTau [] [ClockReset c] [] newLoc
    let newEdge2 =
          Edge newLoc CTTau [ClockConstraint c GE x] [] [] (toLocation s')
    -- update state
    put $ TABS (n + 1) (update ta [newLoc] [newEdge1, newEdge2] [newInv]) beh
    -- deal with other transitions outgoing from s
    cToTAAddTimeouts''' newLoc $ outgoing ts s
    return ()

update :: VTA
       -> [VLocation]
       -> [VTEdge]
       -> [(VLocation, [ClockConstraint])]
       -> VTA
update (TimedAutomaton i ls l0 cls uls cs vs as es is) newL newE newI =
  TimedAutomaton i (newL ++ ls) l0 cls uls cs vs as (newE ++ es) (newI ++ is)

cToTAAddTimeouts' :: [(VState, VTransition)] -> MS.State TABuildState ()
cToTAAddTimeouts' []             = return ()
cToTAAddTimeouts' ((s, tt) : ss) = do
  cToTAAddTimeouts'' s tt
  cToTAAddTimeouts' ss
  return ()

cToTAAddTimeouts :: MS.State TABuildState ()
cToTAAddTimeouts = do
  currentState <- get
  let ts  = transitions . beh $ currentState
  let ss  = states . beh $ currentState
  let ss' = catMaybes $ traverseToSnd (getTimeoutTransition ts) <$> ss
  cToTAAddTimeouts' ss'
  return ()

cToTAAddRegular'' :: [VTransition] -> MS.State TABuildState ()
cToTAAddRegular'' []       = return ()
cToTAAddRegular'' (t : ts) = do
  TABS n ta beh <- get
  let s  = source t
  let s' = target t
  let l  = label t
  case label t of
    InternalLabel (TimeValue 0) InfiniteValue -> do
      let newEdge = Edge (toLocation s) CTTau [] [] [] (toLocation s')
      put $ TABS n (update ta [] [newEdge] []) beh
      return ()
    EventLabel e -> if isInput e
      then do
        let newLoc   = Location ("_" ++ show n)
        let newEdge1 = Edge (toLocation s) CTTau [] [] [] newLoc
        let newEdge2 = Edge newLoc (liftToCTIOEvent e) [] [] [] (toLocation s')
        put $ TABS (n + 1) (update ta [newLoc] [newEdge1, newEdge2] []) beh
        return ()
      else do
        let newEdge =
              Edge (toLocation s) (liftToCTIOEvent e) [] [] [] (toLocation s')
        put $ TABS n (update ta [] [newEdge] []) beh
        return ()
    _ -> return ()

cToTAAddRegular' :: [[VTransition]] -> MS.State TABuildState ()
cToTAAddRegular' []         = return ()
cToTAAddRegular' (ts : tss) = do
  cToTAAddRegular'' ts
  cToTAAddRegular' tss
  return ()

cToTAAddRegular :: MS.State TABuildState ()
cToTAAddRegular = do
  currentState <- get
  let ts = transitions . beh $ currentState
  let ss = states . beh $ currentState
  let st = outgoing ts <$> filter (not . hasTimeoutTransition ts) ss
  cToTAAddRegular' st
  return ()

cToTAAddInternals' :: [VTransition] -> MS.State TABuildState ()
cToTAAddInternals' [] = return ()
cToTAAddInternals' (t:ts) = do
  TABS n ta beh <- get
  let c  = Clock "0"
  let s  = source t
  let s' = target t
  let l  = label t
  case label t of
    (InternalLabel (TimeValue x) (TimeValue y)) -> do
      -- add new location and its invariant
      let newLoc           = Location ("_" ++ show n)
      let newInv           = (newLoc, [ClockConstraint c LE y])
      -- add edges
      let newEdge1 = Edge (toLocation s) CTTau [] [ClockReset c] [] newLoc
      let newEdge2 =
            Edge newLoc CTTau [ClockConstraint c GE x] [] [] (toLocation s')
      put $ TABS (n+1) (update ta [newLoc] [newEdge1, newEdge2] [newInv]) beh
      return ()
    _ -> return ()
  cToTAAddInternals' ts
  return ()

cToTAAddInternals :: MS.State TABuildState ()
cToTAAddInternals = do
  currentState <- get
  let ts = transitions . beh $ currentState
  let tss = filter (isRegularInternalLabel . label) ts
  cToTAAddInternals' tss
  return ()

hasTimeoutTransition :: [VTransition] -> VState -> Bool
hasTimeoutTransition ts s = isJust $ getTimeoutTransition ts s

getTimeoutTransition :: [VTransition] -> VState -> Maybe VTransition
getTimeoutTransition ts s = listToMaybe $ filter timeoutT $ outgoing ts s
  where timeoutT = isTimeoutLabel . label

-- |Transform a component into a timed automaton (state monadic helper)
cToTA' :: MS.State TABuildState ()
cToTA' = do
  -- treatment of final states
  cToTAAddFinals
  -- treatment of timeouts
  cToTAAddTimeouts
  -- treatment of events and infinite internal events
  cToTAAddRegular
  -- treatment of regular (non infinite) internal events
  cToTAAddInternals
  -- end
  return ()

-- |Transform a component into a timed automaton (calls the state monadic helper)
cToTA :: ComponentInstance -> VTA
cToTA (ComponentInstance i (BasicComponent _ _ b)) =
  let ta0 = TimedAutomaton i ls l0 cls uls cs vs as es is
  in 
    -- initialize with steps 1 to 3.
      addObservers . ta . snd $ runState cToTA' (TABS 0 ta0 b)
 where
  ls     = lsorig
  l0     = toLocation (initialState b)
  cls    = []
  uls    = lsorig
  cs     = [Clock "0"]
  vs     = empty
  as     = events (alphabet b) ++ [CTTau]
  es     = []
  is     = []
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
      ok ci o' jp = (jpname jp == instanceId ci) && (jpoperation jp == o')
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
