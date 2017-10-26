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
    MessageType(..)
  , Message(..)
  , Operation(..)
  , Signature(..)
  , TimeConstraint(..)
  , JoinPoint(..)
  , Binding(..)
  , Component(..)
  , VEvent
  , VState
  , VLocation
  , VTransition
  , VPath
  , VLTS
  , VCTree
  , VTA
  , VEdge
  , VTATree
  , self
    -- * validity checking
  , isValidSignature
  , isValidBehavior
  , isValidTimeConstraint
  , isValidComponent
    -- * model transformation
  , cToTATree
  , cTreeToTATree
  , cToCTree
  , cToTA
  , genClock
    -- * other
  , isCSource
  , isCTarget
  , isCPath)
where

import           Data.Bifunctor                  (second)
import           Data.Hashable                   (Hashable, hash, hashWithSalt)
import           Data.Map                        as M (Map, keysSet)
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
import           Models.TimedAutomaton           as TA (Clock (..),
                                                        ClockConstraint (..),
                                                        ClockOperator (GE, LE),
                                                        ClockReset (..),
                                                        Edge (Edge),
                                                        Location (..),
                                                        TimedAutomaton (..),
                                                        ToXta, asXta)
import           Numeric.Natural
import           Trees.Tree
import           Trees.Trifunctor                (first)

-- |self is a specific name.
self :: Name
self = Name []

-- |A communication input-output event defined over operations.
type VEvent = CIOEvent Operation

-- |A state over a String
type VState = State String

-- |A location over a String
type VLocation = Location String

-- |A transition where actions are VEvents and states are Strings
type VTransition = Transition VEvent String

-- |A path where actions are VEvents and states are Strings
type VPath = Path VEvent String

-- |An LTS where actions are VEvents and states are Strings
type VLTS = LabelledTransitionSystem VEvent String

-- |A tree with components in leaves, components in nodes, indexed by names
type VCTree = Tree Component Component Name

-- |A timed automaton where actions are VEvents and locations are Strings
type VTA = TimedAutomaton VEvent String

-- |An edge where actions are VEvents and locations are Strings
type VEdge = Edge VEvent String

-- |A tree with components with (possibly) VTA in leaves, components in nodes, indexed by names
type VTATree = Tree VTA Component Name

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

-- |A time constraint is used to specify a minimum and maximum time interval
-- between two events (a start event and an end event).
data TimeConstraint =
  TimeConstraint {startEvent :: VEvent
                 ,stopEvent  :: VEvent
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
data Component
  = BasicComponent {componentId     :: Name
                   ,signature       :: Signature
                   ,behavior        :: VLTS
                   ,timeconstraints :: [TimeConstraint]
                   }
  | CompositeComponent {componentId :: Name
                       ,signature   :: Signature
                       ,children    :: [(Name, Component)]
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
  | getAny $ foldMap (Any . elem' ps) rs = False
  | keysSet fi /= os = False
  | keysSet fo /= os = False
  | otherwise = True
  where os = S.fromList $ ps <> rs
        xs `elem'` x = x `elem` xs

-- |Check the validity of a behavior with reference to a signature.
-- A behavior is valid with reference to a signature iff:
-- - it is valid in the sense of LTS, and
-- - TODO the alphabet is the smallest set such that ...
isValidBehavior :: Signature -> VLTS -> Bool
isValidBehavior s b@(LabelledTransitionSystem as ss i fs ts) = isValidLTS b

-- |Check the validity of a time constraint with reference to a behavior.
-- A time constraint is valid with reference to a behavior b iff:
-- - beginTime >=0 and endTime >= 0,
-- - beginTime < endTime, and
-- - beginEvent and endEvent are in the alphabet of b.
isValidTimeConstraint :: VLTS -> TimeConstraint -> Bool
isValidTimeConstraint b (TimeConstraint a1 a2 t1 t2)
  | t1 >= t2 = False
  | a1 `notElem` alphabet b = False
  | a2 `notElem` alphabet b = False
  | otherwise = True

-- |Check the validity of a component.
-- A basic component is valid iff:
-- - its id is valid,
-- - its signature is valid,
-- - its behavior is valid with reference to its signature,
-- - each of its time constraints is valid with reference to its behavior, and
-- - if there is at least a time contraint then there is no loop in the behavior.
-- TODO A composite component is valid iff ...
isValidComponent :: Component -> Bool
isValidComponent (BasicComponent i s b tcs) = cond0 && cond1 && cond2 && cond3 && cond4
  where cond0 = isValidName i
        cond1 = isValidSignature s
        cond2 = isValidBehavior s b
        cond3 = getAll $ foldMap (All . isValidTimeConstraint b) tcs
        cond4 = null tcs || (not . hasLoop) b
isValidComponent (CompositeComponent i s cs ibs ebs) = True -- TODO

-- |Check if a transition is a possible source for a time constraint.
isCSource
  :: TimeConstraint -> VTransition -> Bool
isCSource k t = label t == startEvent k

-- |Check if a transition is a possible source for a time constraint (Maybe version).
isCSource'
  :: TimeConstraint -> Maybe VTransition -> Bool
isCSource' _ Nothing  = False
isCSource' k (Just t) = isCSource k t

-- |Check if a transition is a possible target for a time constraint.
isCTarget
  :: TimeConstraint -> VTransition -> Bool
isCTarget k t = label t == stopEvent k

-- |Check if a transition is a possible target for a time constraint (Maybe version).
isCTarget'
  :: TimeConstraint -> Maybe VTransition -> Bool
isCTarget' _ Nothing  = False
isCTarget' k (Just t) = isCTarget k t

-- |Check if a state is possibly concerned by a time constraint
-- a state s is concerned by a time constraint k wrt a behavior b
-- iff there is a path p = t1 t2 ... tn-1 tn in b such that:
-- - p is a path for k, and
-- - s is the source state of a transition in t2 ... tn-1 tn
isCState :: VLTS -> TimeConstraint -> VState -> Bool
isCState b k s =
  getAny $ foldMap (Any . isCPathForState k s) (paths' b)
  where isCPathForState _ _ (Path []) = False
        isCPathForState k' s' p@(Path (t1:ts)) =
          isCPath k' p && getAny (foldMap (Any . (== s') . source) ts)

-- |Check if a path is possibly concerned by a time constraint.
-- That is, if the path:
-- - starts with a transition that is a possible source for the time constraint, and
-- - ends with a transition that is a possible target for the time constraint.
isCPath :: TimeConstraint -> VPath -> Bool
isCPath k p = isCSource' k (start p) && isCTarget' k (end p)

-- |Transform an architecture (given as a component) into a timed automaton tree
cToTATree :: Component -> VTATree
cToTATree = cTreeToTATree . cToCTree

-- |Transform an architecture (given as a component) into a component tree
cToCTree :: Component -> VCTree
cToCTree c@BasicComponent{}                = Leaf c
cToCTree c@(CompositeComponent _ _ cs _ _) = Node c cs' where cs' = second cToCTree <$> cs

-- |Transform a component tree into a timed automaton tree
cTreeToTATree :: VCTree -> VTATree
cTreeToTATree = mapleaves cToTA

-- |Transform a component into a timed automaton
cToTA :: Component -> VTA
cToTA (BasicComponent i s b cts) = TimedAutomaton i ls l0 cs as es is
  where ls = toLocation            <$> states b
        l0 = toLocation            (initialState b)
        cs = genClock              <$> cts
        as = alphabet b
        es =
          (genEdge cts             <$> transitions b) ++
          (genLoopOn . toLocation  <$> finalStates b)
        is = genInvariant cts b    <$> states b
cToTA CompositeComponent{} = undefined

-- |Transform a state into a location
toLocation :: VState -> VLocation
toLocation (State s) = Location s

-- |Generate a clock for a time constraint
genClock :: TimeConstraint -> Clock
genClock = Clock . (\h -> if h>=0 then show h else '_' : show (-h)) . hash

-- |Generate an edge for a transition
genEdge :: [TimeConstraint] -> VTransition -> VEdge
genEdge ks t@(Transition s1 a s2) = Edge s1' a g r s2'
  where s1' = toLocation s1
        g = [genCBegin k |k <- filter (`isCTarget` t) ks]
        r = [genReset k|k <- filter (`isCSource` t) ks]
        s2' = toLocation s2

-- |Generate a looping tau edge for a state
genLoopOn :: VLocation -> VEdge
genLoopOn l = Edge l CTau [] [] l

-- |Generate a reset for the clock of a time constraint
genReset :: TimeConstraint -> ClockReset
genReset = ClockReset . genClock

-- |Generate the invariant for a state
genInvariant
  :: [TimeConstraint] -> VLTS -> VState -> (VLocation, [ClockConstraint])
genInvariant ks b s = (toLocation s, [genCEnd k | k <- ks, isCState b k s])

-- |Generate a clock constraint "clock GE beginTime" from a time constraint
genCBegin
  :: TimeConstraint -> ClockConstraint
genCBegin k = ClockConstraint (genClock k) GE (beginTime k)

-- |Generate a clock constraint "clock LE endTime" from a time constraint
genCEnd
  :: TimeConstraint -> ClockConstraint
genCEnd k = ClockConstraint (genClock k) LE (endTime k)

--
-- more or less documented
--



-- | Helpers

mapleaves :: (a -> a') -> Tree a b c -> Tree a' b c
mapleaves = first

