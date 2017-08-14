{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Veca.Veca
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Core types and functions for Veca.
-----------------------------------------------------------------------------

module Veca.Veca (
    -- * constructors
    Name(..)
  , MessageType(..)
  , Message(..)
  , Operation(..)
  , Signature(..)
  , TimeConstraint(..)
  , JoinPoint(..)
  , Binding(..)
  , Component(..)
  , ComponentTree
  , VecaEvent
  , VecaTransition
  , VecaLTS
  , VecaEdge
  , VecaTA
  , VecaTATree
    -- * validity checking
  , isValidSignature
  , isValidBehavior
  , isValidTimeConstraint
  , isValidComponent
    -- * model transformation
  , component2taTree
  , component2componentTree
  , component2ta
    -- * other
  , tcsource
  , tctarget)
where

import           GHC.Generics                    (Generic)
import           Data.Hashable                   (Hashable, hashWithSalt, hash)
import           Data.Map                        as M (Map, fromListWith,
                                                       keysSet, toList)
import           Data.Monoid                     (All (..), Any (..), (<>))
import           Data.Set                        as S (fromList)
import           Models.Events                   (CIOEvent (..), CIOLTS)
import           Models.LabelledTransitionSystem (LabelledTransitionSystem (..),
                                                  Path (..), State (..),
                                                  Transition (..), hasLoop,
                                                  isValidLTS, pathStates)
import           Models.TimedAutomaton           as TA
import           Numeric.Natural
import           Trees.Tree
import           Trees.Trifunctor

-- |A name (including a specific name, Self) is a string.
data Name
  = Name String
  | Self
  deriving (Eq,Ord,Show,Generic)

-- |ToXta instance for names.
instance ToXta Name where
  asXta (Name s) = s
  asXta Self = "self"

-- |Hash for names.
instance Hashable Name

-- |A message type is a string.
-- It can be more or less complex, e.g., "foo" or "{x:Integer,y:String}".
newtype MessageType
  = MessageType String
  deriving (Eq,Ord,Show)

-- |A message is a name and a message type.
data Message
  = Message {messagename :: Name, messagetype :: MessageType}
  deriving (Eq,Ord,Show)

-- |An operation is a name.
newtype Operation
  = Operation Name
  deriving (Eq,Ord,Show,Generic)

-- |Hask for operations.
instance Hashable Operation

-- |ToXta instance for operations.
instance ToXta Operation where
  asXta (Operation n)= asXta n

-- |A signature is given as:
-- - a set of provided operations,
-- - a set of required operations,
-- - a mapping from operations to (input) messages, and
-- - a partial mapping from operations to (output) messages.
data Signature =
  Signature {providedOperations :: [Operation]
            ,requiredOperations :: [Operation]
            ,input              :: Map Operation Message
            ,output             :: Map Operation (Maybe Message)
            }
  deriving (Show)

-- |A VECA event is a communication input-output event defined over operations.
type VecaEvent = CIOEvent Operation

type VecaTransition a = Transition VecaEvent a
-- |A VECA transition is a transition defined over VECA events.

type VecaLTS a = CIOLTS Operation a
-- |A VECA LTS is a communication input-output LTS defined over operations.

-- |A time constraint is used to specify a minimum and maximum time interval
-- between two events (a start event and an end event).
data TimeConstraint =
  TimeConstraint {startEvent :: VecaEvent
                 ,stopEvent  :: VecaEvent
                 ,beginTime  :: Natural
                 ,endTime    :: Natural
                 }
  deriving (Eq,Ord,Show)

-- |Hash for time constraints.
instance Hashable TimeConstraint where
  hashWithSalt s (TimeConstraint e1 e2 d1 d2) =
    s `hashWithSalt`
    e1 `hashWithSalt`
    e2 `hashWithSalt`
    d1 `hashWithSalt` d2

-- |A join point is a name (possibly Self) and an operation.
data JoinPoint
  = JoinPoint {name      :: Name
              ,operation :: Operation
              }
    deriving (Eq,Ord)

-- |Show instance for join points.
instance Show JoinPoint where
  show (JoinPoint n o) =
    show o <> "#" <> show n

-- |A binding relates two operations in two components.
-- It can be internal or external.
data Binding
  = InternalBinding {from :: JoinPoint, to :: JoinPoint}
  | ExternalBinding {from :: JoinPoint, to :: JoinPoint}
  deriving (Eq,Ord)

-- |Show instance for bindings.
instance Show Binding where
  show (InternalBinding j1 j2) =
    show j1 <> ">--<" <> show j2
  show (ExternalBinding j1 j2) =
    show j1 <> "<-->" <> show j2

-- |A component is either a basic or a composite component.
-- A basic component is given as:
-- - an id,
-- - a signature,
-- - a behavior, and
-- - time constraints.
-- A composite component is given as:
-- - an id,
-- - a signature,
-- - sub-components (aka children),
-- - internal bindings, and
-- - external bindings.
-- TODO checking
data Component a
  = BasicComponent {componentId     :: String
                   ,signature       :: Signature
                   ,behavior        :: VecaLTS a
                   ,timeconstraints :: [TimeConstraint]
                   }
  | CompositeComponent {componentId :: String
                       ,signature   :: Signature
                       ,children    :: Map Name (Component a)
                       ,inbinds     :: [Binding]
                       ,extbinds    :: [Binding]
                       }
  deriving (Show)

-- |Check the validity of a signature.
-- A Signature is valid iff:
-- - the sets of provided and required operations are disjoint,
-- - the domain of input is the set of operations (provided and required), and
-- - the domain of output is the set of operations (provided and required).
isValidSignature :: Signature -> Bool
isValidSignature (Signature ps rs fi fo)
  | getAny $ foldMap (Any . (elem' ps)) rs = False
  | keysSet fi /= os = False
  | keysSet fo /= os = False
  | otherwise = True
  where os = S.fromList $ ps <> rs
        xs `elem'` x = x `elem` xs

-- |Check the validity of a behavior with reference to a signature.
-- A behavior is valid with reference to a signature iff:
-- - it is valid in the sense of LTS, and
-- - TODO the alphabet is the smallest set such that ...
isValidBehavior :: (Ord a) => Signature -> VecaLTS a -> Bool
isValidBehavior s b@(LabelledTransitionSystem as ss i fs ts) = isValidLTS b

-- |Check the validity of a time constraint with reference to a behavior.
-- A time constraint is valid with reference to a behavior b iff:
-- - beginTime >=0 and endTime >= 0,
-- - beginTime < endTime, and
-- - beginEvent and endEvent are in the alphabet of b.
isValidTimeConstraint :: VecaLTS a -> TimeConstraint -> Bool
isValidTimeConstraint b (TimeConstraint a1 a2 t1 t2)
  | t1 >= t2 = False
  | not $ a1 `elem` alphabet b = False
  | not $ a2 `elem` alphabet b = False
  | otherwise = True

-- |Check the validity of a component.
-- A basic component is valid iff:
-- - its id is valid,
-- - its signature is valid,
-- - its behavior is valid with reference to its signature,
-- - each of its time constraints is valid with reference to its behavior, and
-- - if there is at least a time contraint then there is no loop in the behavior.
-- TODO A composite component is valid iff ...
isValidComponent :: (Ord a) => Component a -> Bool
isValidComponent (BasicComponent i s b tcs) = cond0 && cond1 && cond2 && cond3 && cond4
  where cond0 = length i > 0
        cond1 = isValidSignature s
        cond2 = isValidBehavior s b
        cond3 = getAll $ foldMap (All . isValidTimeConstraint b) tcs
        cond4 = null tcs || (not . hasLoop) b
isValidComponent (CompositeComponent i s cs ibs ebs) = True -- TODO

-- |Check if a transition is a possible source for a time constraint.
possibleTCSource :: TimeConstraint -> VecaTransition a -> Bool
possibleTCSource k t = (label t) == (startEvent k)

-- |Check if a transition is a possible target for a time constraint.
possibleTCTarget :: TimeConstraint -> VecaTransition a -> Bool
possibleTCTarget k t = (label t) == (stopEvent k)

tcsource
-- |Get all transitions in a behavior that are possible sources of a time constraint.
  :: VecaLTS a -> TimeConstraint -> [VecaTransition a]
tcsource (LabelledTransitionSystem _ _ _ _ ts) k =
  filter (possibleTCSource k) ts

tctarget
-- |Get all transitions in a behavior that are possible targets of a time constraint.
  :: VecaLTS a -> TimeConstraint -> [VecaTransition a]
tctarget (LabelledTransitionSystem _ _ _ _ ts) k =
  filter (possibleTCTarget k) ts

-- |Get all paths that begin with a transition in 'tcsource'
-- and end with a transition in 'tctarget'.
tcpaths :: VecaLTS a -> TimeConstraint -> [Path (CIOEvent Operation) a]
tcpaths l k = undefined -- TODO

--
-- more or less documented
-- still experimental
--

-- |A ComponentTree is a 'Tree' with 'Component's in nodes and leaves, and with subtrees indexed by 'Name's.
type ComponentTree a = Tree (Component a) (Component a) Name

-- |A VecaTA is a 'TimedAutomaton' defined over 'VecaEvent's.
type VecaTA = TA VecaEvent

-- |A VecaEdge is an 'Edge' defined over 'VecaEvent's.
type VecaEdge = Edge VecaEvent

-- |A VecaTATree is a 'Tree' with 'VecaTA's in leaves, 'Component's in nodes, and with subtrees indexed by 'Name's.
type VecaTATree a = Tree (Maybe (VecaTA a)) (Component a) Name

-- |Transform an architecture (given as a component) into a timed automaton tree
component2taTree :: Ord a => Component a -> VecaTATree a
component2taTree = componentTree2taTree . component2componentTree

-- |Transform a component tree into a timed automaton tree
componentTree2taTree :: Ord a => ComponentTree a -> VecaTATree a
componentTree2taTree = mapleaves component2ta

-- |Transform an architecture (given as a component) into a component tree
component2componentTree :: Component a -> ComponentTree a
component2componentTree c@BasicComponent{} = Leaf c
component2componentTree c@(CompositeComponent _ _ cs _ _) =
  Node c (M.toList $ component2componentTree <$> cs)

-- |Transform a component into a timed automaton
component2ta :: (Ord a)
             => Component a -> Maybe (VecaTA a)
component2ta (BasicComponent i s b cts) =
  Just (TimedAutomaton i ls l0 cs as es is)
  where ls = trStateToLocation <$> states b
        l0 = trStateToLocation $ initialState b
        cs = trTimeConstraintToClock <$> cts
        as = alphabet b
        es =
          ((genTransitionForEdge b cts) <$> transitions b) <>
          (genLoopForState <$> finalStates b)
        is = genInvariants b cts
component2ta CompositeComponent{} = Nothing

-- | Helpers

mapleaves :: (a -> a') -> (Tree a b c) -> (Tree a' b c)
mapleaves = first

-- | Transform an LTS state to a TimedAutomaton location
trStateToLocation :: State a -> Location a
trStateToLocation (State s) = Location s

-- | Generate a TimedAutomaton looping tau edge for an LTS state
genLoopForState :: State a -> VecaEdge a
genLoopForState s = Edge l CTau [] [] l
  where l = trStateToLocation s

genTransitionForEdge :: VecaLTS b -> [TimeConstraint] -> VecaTransition b -> VecaEdge b
genTransitionForEdge b ks t@(Transition s1 a s2) =
  Edge (trStateToLocation s1)
       a
       (trTimeConstraintToClockConstraint2 <$>
        (filter (flip possibleTCTarget $ t) ks))
       (trTimeConstraintToClockReset <$>
        (filter (flip possibleTCSource $ t) ks))
       (trStateToLocation s2)

-- | Generate the invariants of a TimedAutomaton from an LTS
genInvariants
  :: Ord a
  => VecaLTS a -> [TimeConstraint] -> Map (Location a) [ClockConstraint]
genInvariants lts ks =
  fromListWith (++) $ foldMap (genInvariants' lts) ks

genInvariants'
  :: Ord a
  => VecaLTS a
  -> TimeConstraint
  -> [(Location a,[ClockConstraint])]
genInvariants' lts k =
  foldMap (genClockConstraintForLocation k . trStateToLocation)
          (foldMap pathStates $ tcpaths lts k)

genClockConstraintForLocation
  :: TimeConstraint -> Location a -> [(Location a,[ClockConstraint])]
genClockConstraintForLocation k l =
  [(l,[trTimeConstraintToClockConstraint1 k])]

trTimeConstraintToClockConstraint1
  :: TimeConstraint -> ClockConstraint
trTimeConstraintToClockConstraint1 k =
  (ClockConstraint (trTimeConstraintToClock k)
                   TA.LE
                   (endTime k))

trTimeConstraintToClockConstraint2
  :: TimeConstraint -> ClockConstraint
trTimeConstraintToClockConstraint2 k =
  (ClockConstraint (trTimeConstraintToClock k)
                   TA.GE
                   (beginTime k))

trTimeConstraintToClock :: TimeConstraint -> Clock
trTimeConstraintToClock = Clock . show

trTimeConstraintToClockReset :: TimeConstraint -> ClockReset
trTimeConstraintToClockReset = ClockReset . trTimeConstraintToClock
