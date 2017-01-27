----------------------------------------------------------------------------
-- |
-- Module      :  Veca
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Core types and functions for Veca.
-----------------------------------------------------------------------------

module Veca (-- * constructors
              Message(..)
            , Operation(..)
            , Name(..)
            , Signature(..)
            , TimeConstraint(..)
            , JoinPoint(..)
            , Binding(..)
            , Component(..)
            -- * instantiated types
            , BehaviorEvent
            , Behavior
            -- * validity checking
            , isValidSignature
            , isValidBehavior
            , isValidTimeConstraint
            , isValidComponent
            -- * other
            , tcsource
            , tctarget
            )
where

import           Data.Map                 as M (Map, fromList, keysSet, map,
                                                toList)
import           Data.Monoid              as DM (mappend)
import           Data.Set                 as S (Set, empty, filter, foldr,
                                                intersection, map, member, null,
                                                union)
import           LabelledTransitionSystem as L
import           Numeric.Natural
import           TimedAutomaton           as TA
import           Tree                     as T
import           Trifunctor               as TF

-- |A name. This is the encapsulation of a String, or Self.
data Name
  = Name String
  | Self
  deriving (Eq,Ord,Show)

-- |A message. This is the encapsulation of a String.
data Message
  = Message String
  deriving (Eq,Ord,Show)

-- |An operation. This is the encapsulation of a String.
data Operation
  = Operation String
  deriving (Eq,Ord,Show)

-- |A signature is given as
-- a set of provided operations,
-- a set of required operations,
-- a mapping from operations to (input) messages, and
-- a partial mapping from operations to (output) messages.
data Signature =
  Signature {providedOperations :: Set Operation                 -- ^ set of the provided operations
            ,requiredOperations :: Set Operation                 -- ^ set of the required operations
            ,input              :: Map Operation Message         -- ^ input messages of operations
            ,output             :: Map Operation (Maybe Message) -- ^ output messages of operations
            }
  deriving (Show)

-- |A behavior is a 'CIOLTS' defined over 'Operation's
type Behavior a = CIOLTS Operation a

-- |A communication event based on 'Operation's.
type BehaviorEvent = CIOEvent Operation

-- |A time constraint is used to specify a minimum and maximum time interval
-- between two events
data TimeConstraint =
  TimeConstraint {startEvent :: BehaviorEvent -- ^ first event
                 ,stopEvent  :: BehaviorEvent -- ^ second event
                 ,beginTime  :: Natural          -- ^ minimum time interval
                 ,endTime    :: Natural          -- ^ maximum time interval
                 }
  deriving (Eq,Ord,Show)

-- |A join point is a name and an operation
data JoinPoint
  = JoinPoint {name      :: Name      -- ^ component concerned by the join point
              ,operation :: Operation -- ^ operation concerned by the join point
              }
    deriving (Eq,Ord)

instance Show JoinPoint where
  show (JoinPoint n o) =
    show o `DM.mappend` "◊" `DM.mappend` show n

-- |A binding relates two operations in two components.
-- It can be internal or external.
data Binding
  = InternalBinding {from :: JoinPoint, to :: JoinPoint}
  | ExternalBinding {from :: JoinPoint, to :: JoinPoint}
  deriving (Eq,Ord)

instance Show Binding where
  show (InternalBinding j1 j2) =
    show j1 `DM.mappend` ">--<" `DM.mappend` show j2
  show (ExternalBinding j1 j2) =
    show j1 `DM.mappend` "<-->" `DM.mappend` show j2

-- |A component is either a basic or a composite component.
-- A basic component is given as a signature, a behavior, and time constraints.
-- A composite component ... TODO
data Component a
  = BasicComponent {signature       :: Signature          -- ^ signature
                   ,behavior        :: Behavior a         -- ^ behavior
                   ,timeconstraints :: Set TimeConstraint -- ^ time constraints
                   }
  | CompositeComponent {signature :: Signature              -- ^ signature
                       ,children  :: Map Name (Component a) -- ^ typed subcomponents
                       ,inbinds   :: Set Binding            -- ^ internal bindings
                       ,extbinds  :: Set Binding            -- ^ external bindings
                       }
  deriving (Show)

-- |Check the validity of a 'Signature'.
--
-- A 'Signature' is valid iff:
--
-- - the sets of provided and required operations are disjoint
-- - the domain of input is the set of operations (provided and required)
-- - the domain of output is the set of operations (provided and required)
isValidSignature :: Signature -> Bool
isValidSignature (Signature ps rs fi fo)
  | not $ S.null (ps `S.intersection` rs) = False
  | M.keysSet fi /= os || M.keysSet fo /= os = False
  | otherwise = True
  where os = ps `S.union` rs

-- |Check the validity of a 'Behavior' with reference to a 'Signature'.
--
-- A 'Behavior' is valid with reference to a 'Signature' iff:
--
-- - it is valid in the sense of 'LTS'
-- - the alphabet is the smallest set such that:
-- - - ... TODO
isValidBehavior :: (Ord a) => Signature -> Behavior a -> Bool
isValidBehavior s b@(LabelledTransitionSystem as ss i fs ts) = isValidLTS b

-- |Check the validity of a 'TimeConstraint' with reference to a 'Behavior'.
--
-- A 'TimeConstraint' is valid with reference to a 'Behavior' b iff:
--
-- - beginTime >=0 and endTime >= 0
-- - beginTime < endTime
-- - beginEvent and endEvent are in the alphabet of b
isValidTimeConstraint :: Behavior a -> TimeConstraint -> Bool
isValidTimeConstraint b (TimeConstraint a1 a2 t1 t2)
  | t1 >= t2 = False
  | not $ a1 `S.member` alphabet b = False
  | not $ a2 `S.member` alphabet b = False
  | otherwise = True

-- |Check the validity of a 'Component'.
--
-- A 'BasicComponent' is valid iff:
--
-- - its signature is valid
-- - its behavior is valid with reference to its signature
-- - each of its time constraints is valid with reference to its behavior
-- - if there is at least a time contraint then there is not loop
--
-- A 'CompositeComponent' is valid iff: TODO
isValidComponent :: (Ord a) => Component a -> Bool
isValidComponent (BasicComponent s b tcs) = cond1 && cond2 && cond3 && cond4
  where cond1 = isValidSignature s
        cond2 = isValidBehavior s b
        cond3 = S.foldr (&&) True (S.map (isValidTimeConstraint b) tcs)
        cond4 = S.null tcs || (not . hasLoop) b
isValidComponent (CompositeComponent s cs ibs ebs) = True -- TODO

-- |Get all 'Transition's whose label is the start event of a 'TimeConstraint'.
tcsource
  :: Behavior a -> TimeConstraint -> Set (Transition BehaviorEvent a)
tcsource (LabelledTransitionSystem _ _ _ _ ts) (TimeConstraint a1 _ _ _) =
  S.filter ((== a1) . label) ts

-- |Get all 'Transition's whose label is the stop event of a 'TimeConstraint'.
tctarget
  :: Behavior a -> TimeConstraint -> Set (Transition BehaviorEvent a)
tctarget (LabelledTransitionSystem _ _ _ _ ts) (TimeConstraint _ a2 _ _) =
  S.filter ((== a2) . label) ts

-- |Get all paths that begin with a 'Transition' in 'tcsource'
-- and end with a transition in 'tctarget'.
tcpaths ::
  Behavior a -> TimeConstraint -> [Transition BehaviorEvent a]
tcpaths l@(LabelledTransitionSystem _ _ _ _ ts) (TimeConstraint a1 a2 _ _) = [] -- TODO

--
-- not documented
-- experimental
--

-- |Helper types
type CIOTA = TA (CIOEvent Operation)
type CIOEdge = Edge (CIOEvent Operation)
type ComponentTree a = Tree (Component a) (Component a) Name
type CIOTATree a = Tree (Maybe (CIOTA a)) (Component a) Name

-- |Helper functionstoLocation :: State -> Location
toLocation :: State a -> Location a
toLocation (State s) = Location s

toEdge :: Transition a b -> Edge a b
toEdge (Transition s1 a s2) =
  Edge (toLocation s1)
        a
        S.empty
        S.empty
        (toLocation s2)

generateTauLoop :: State a -> CIOEdge a
generateTauLoop s = Edge l CTau S.empty S.empty l
  where l = toLocation s

generateClock :: TimeConstraint -> Clock
generateClock t = Clock (show t)

-- compute_invariants :: State -> (Location, Set ClockConstraint)
-- compute_invariants s = -- TODO

-- |Transform an architecture (given as a component) into a component tree
toComponentTree :: Component a -> ComponentTree a
toComponentTree c@BasicComponent{} = Leaf c
toComponentTree c@(CompositeComponent _ cs _ _) =
  Node c (M.toList (M.map toComponentTree cs))

-- |Transform a component tree into a timed automaton tree
toTimedAutomatonTree :: (Ord a) => ComponentTree a -> CIOTATree a
toTimedAutomatonTree = TF.first toTimedAutomaton

-- |Transform a component into a timed automaton
toTimedAutomaton :: (Ord a) => Component a -> Maybe (CIOTA a)
toTimedAutomaton (BasicComponent s b cts) =
  Just (TimedAutomaton ls l0 cs as es is)
  where
    ss = states b
    ls = S.map toLocation ss
    l0 = toLocation $ initialState b
    cs = S.map generateClock cts
    as = alphabet b
    e0 = S.map toEdge (transitions b)
    es = e0 `S.union` S.map generateTauLoop (finalStates b)
    -- es = ?? -- lignes 7 et 8
    is = M.fromList []
    -- is = M.fromList (S.toList (S.map computeInvariants ss))

toTimedAutomaton CompositeComponent{} = Nothing
