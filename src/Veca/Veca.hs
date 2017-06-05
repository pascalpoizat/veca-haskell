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
    Message(..)
  , Operation(..)
  , Name(..)
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

import           Data.Map                        as M (Map,
                                                       fromListWith, keysSet,
                                                       toList)
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

-- |A name. This is the encapsulation of a String, or Self.
data Name
  = Name String
  | Self
  deriving (Eq,Ord,Show)

-- |A message. This is the encapsulation of a String.
newtype Message
  = Message String
  deriving (Eq,Ord,Show)

-- |An operation. This is the encapsulation of a String.
newtype Operation
  = Operation String
  deriving (Eq,Ord,Show)

-- |ToXta instance for Operation.
instance ToXta (Operation) where
  asXta = show

-- |A signature is given as
-- a set of provided operations, TODO check set
-- a set of required operations, TODO check set
-- a mapping from operations to (input) messages, and
-- a partial mapping from operations to (output) messages.
data Signature =
  Signature {providedOperations :: [Operation]                   -- ^ set of the provided operations
            ,requiredOperations :: [Operation]                   -- ^ set of the required operations
            ,input              :: Map Operation Message         -- ^ input messages of operations
            ,output             :: Map Operation (Maybe Message) -- ^ output messages of operations
            }
  deriving (Show)

-- |A VecaEvent is a 'CIOEvent' defined over 'Operation's.
type VecaEvent = CIOEvent Operation

-- |A VecaTransition is a 'Transition' defined over 'VecaEvent's
type VecaTransition a = Transition VecaEvent a

-- |A VecaBehavior is a 'CIOLTS' defined over 'Operation's
type VecaLTS a = CIOLTS Operation a

-- |A TimeConstraint is used to specify a minimum and maximum time interval
-- between two events
data TimeConstraint =
  TimeConstraint {startEvent :: VecaEvent -- ^ first event
                 ,stopEvent  :: VecaEvent -- ^ second event
                 ,beginTime  :: Natural   -- ^ minimum time interval
                 ,endTime    :: Natural   -- ^ maximum time interval
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
    show o <> "◊" <> show n

-- |A binding relates two operations in two components.
-- It can be internal or external.
data Binding
  = InternalBinding {from :: JoinPoint, to :: JoinPoint}
  | ExternalBinding {from :: JoinPoint, to :: JoinPoint}
  deriving (Eq,Ord)

instance Show Binding where
  show (InternalBinding j1 j2) =
    show j1 <> ">--<" <> show j2
  show (ExternalBinding j1 j2) =
    show j1 <> "<-->" <> show j2

-- |A component is either a basic or a composite component.
-- A basic component is given as an id, a 'Signature', a 'VecaLTS', and 'TimeConstraint's. TODO check set
-- A composite component ... TODO complete + check set
data Component a
  = BasicComponent {componentId     :: String           -- ^ model id
                   ,signature       :: Signature        -- ^ signature
                   ,behavior        :: VecaLTS a        -- ^ behavior
                   ,timeconstraints :: [TimeConstraint] -- ^ time constraints
                   }
  | CompositeComponent {componentId :: String                 -- ^ model id
                       ,signature   :: Signature              -- ^ signature
                       ,children    :: Map Name (Component a) -- ^ typed subcomponents
                       ,inbinds     :: [Binding]              -- ^ internal bindings
                       ,extbinds    :: [Binding]              -- ^ external bindings
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
  | getAny $ foldMap (Any . (elem' ps)) rs = False
  | keysSet fi /= os = False
  | keysSet fo /= os = False
  | otherwise = True
  where os = S.fromList $ ps <> rs
        xs `elem'` x = x `elem` xs

-- |Check the validity of a 'VecaBehavior' with reference to a 'Signature'.
--
-- A 'VecaBehavior' is valid with reference to a 'Signature' iff:
--
-- - it is valid in the sense of 'LTS'
-- - the alphabet is the smallest set such that:
-- - - ... TODO
isValidBehavior :: (Ord a) => Signature -> VecaLTS a -> Bool
isValidBehavior s b@(LabelledTransitionSystem as ss i fs ts) = isValidLTS b

-- |Check the validity of a 'TimeConstraint' with reference to a 'VecaLTS'.
--
-- A 'TimeConstraint' is valid with reference to a 'VecaLTS' b iff:
--
-- - beginTime >=0 and endTime >= 0
-- - beginTime < endTime
-- - beginEvent and endEvent are in the alphabet of b
isValidTimeConstraint :: VecaLTS a -> TimeConstraint -> Bool
isValidTimeConstraint b (TimeConstraint a1 a2 t1 t2)
  | t1 >= t2 = False
  | not $ a1 `elem` alphabet b = False
  | not $ a2 `elem` alphabet b = False
  | otherwise = True

-- |Check the validity of a 'Component'.
--
-- A 'BasicComponent' is valid iff:
--
-- - its name is valid
-- - its signature is valid
-- - its behavior is valid with reference to its signature
-- - each of its time constraints is valid with reference to its behavior
-- - if there is at least a time contraint then there is not loop
--
-- A 'CompositeComponent' is valid iff: TODO
isValidComponent :: (Ord a) => Component a -> Bool
isValidComponent (BasicComponent i s b tcs) = cond0 && cond1 && cond2 && cond3 && cond4
  where cond0 = length i > 0
        cond1 = isValidSignature s
        cond2 = isValidBehavior s b
        cond3 = getAll $ foldMap (All . isValidTimeConstraint b) tcs
        cond4 = null tcs || (not . hasLoop) b
isValidComponent (CompositeComponent i s cs ibs ebs) = True -- TODO

-- |Get all transitions whose label is the start event of a 'TimeConstraint'.
tcsource
  :: VecaLTS a -> TimeConstraint -> [VecaTransition a]
tcsource (LabelledTransitionSystem _ _ _ _ ts) (TimeConstraint a1 _ _ _) =
  filter ((== a1) . label) ts

-- |Get all transitions whose label is the stop event of a 'TimeConstraint'.
tctarget
  :: VecaLTS a -> TimeConstraint -> [VecaTransition a]
tctarget (LabelledTransitionSystem _ _ _ _ ts) (TimeConstraint _ a2 _ _) =
  filter ((== a2) . label) ts

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

mapleaves :: (a -> a') -> (Tree a b c) -> (Tree a' b c)
mapleaves = first

trStateToLocation :: State a -> Location a
trStateToLocation (State s) = Location s

trTransitionToEdge :: Transition a b -> Edge a b
trTransitionToEdge (Transition s1 a s2) =
  Edge (trStateToLocation s1)
       a
       []
       []
       (trStateToLocation s2)

genLoopForState :: State a -> VecaEdge a
genLoopForState s = Edge l CTau [] [] l
  where l = trStateToLocation s

trTimeConstraintToClock :: TimeConstraint -> Clock
trTimeConstraintToClock t = Clock (show t)

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
        es   -- TODO update wrt lines 7-8 of the algorithm
           =
          (trTransitionToEdge <$> transitions b) <>
          (genLoopForState <$> finalStates b)
        is = computeInvariants b cts
component2ta CompositeComponent{} = Nothing

-- | Helpers

computeInvariants
  :: Ord a
  => VecaLTS a
  -> [TimeConstraint]
  -> Map (Location a) [ClockConstraint]
computeInvariants lts ks =
  fromListWith (++) $ foldMap generator ks
  where
    generator = computeInvariants' lts

computeInvariants'
  :: Ord a
  => VecaLTS a
  -> TimeConstraint
  -> [(Location a,[ClockConstraint])]
computeInvariants' lts k =
  foldMap generator items
  where
    generator = genClockConstraintForLocation k . trStateToLocation
    items = foldMap pathStates $ tcpaths lts k

genClockConstraintForLocation
  :: TimeConstraint -> Location a -> [(Location a,[ClockConstraint])]
genClockConstraintForLocation k l =
  [(l,[trTimedConstraintToClockConstraint k])]

trTimedConstraintToClockConstraint
  :: TimeConstraint -> ClockConstraint
trTimedConstraintToClockConstraint k =
  (ClockConstraint (trTimeConstraintToClock k)
                   TA.LT
                   (endTime k))
