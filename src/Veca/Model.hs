{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Veca.Model
Description : Core types for VECA
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Veca.Model (
    -- * constructors
    MessageType(..)
  , Message(..)
  , Operation(..)
  , Signature(..)
  , TimeConstraint(..)
  , JoinPoint(..)
  , BindingType(..)
  , Binding(..)
  , Component(..)
  , ComponentInstance(..)
  , VName
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
  , VOSubstitution
  , VESubstitution
  , self
    -- * validity checking
  , isValidSignature
  , isValidBehavior
  , isValidTimeConstraint
  , isValidComponent)
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

-- |self is a specific name.
self :: VName
self = Name []

-- |A name over Strings
type VName = Name String

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

-- |A tree with component instances in leaves, component instances in nodes, indexed by names
type VCTree = Tree ComponentInstance ComponentInstance VName

-- |A timed automaton where actions are VEvents and locations are Strings
type VTA = TimedAutomaton VEvent String

-- |An edge where actions are VEvents and locations are Strings
type VEdge = Edge VEvent String

-- |A tree with VTA in leaves, component instances in nodes, indexed by names
type VTATree = Tree VTA ComponentInstance VName

-- |A substitution of operations
type VOSubstitution = Substitution Operation

-- |A substitution of events
type VESubstitution = Substitution VEvent

-- |A message type is a string.
-- It can be more or less complex, e.g., "foo" or "{x:Integer,y:String}".
newtype MessageType =
  MessageType String
  deriving (Eq, Ord, Show, Generic)

{-|
FromJSON instance for message types.
-}
instance FromJSON MessageType

{-|
ToJSON instance for message types.
-}
instance ToJSON MessageType

-- |A message is a name and a message type.
data Message = Message
  { messagename :: VName
  , messagetype :: MessageType
  } deriving (Eq, Ord, Show, Generic)

{-|
FromJSON instance for messages.
-}
instance FromJSON Message

{-|
ToJSON instance for messages.
-}
instance ToJSON Message

-- |An operation is a name.
newtype Operation =
  Operation VName
  deriving (Eq, Ord, Show, Generic)

-- |Hask for operations.
instance Hashable Operation

{-|
FromJSON instance for operations.
-}
instance FromJSON Operation

{-|
ToJSON instance for operations.
-}
instance ToJSON Operation

{-|
FromJSONKey instance for operations.
-}
instance FromJSONKey Operation

{-|
ToJSONKey instance for operations.
-}
instance ToJSONKey Operation

-- |ToXta instance for operations.
instance ToXta Operation where
  asXta (Operation n) = asXta n

-- |A signature is given as:
-- - a set of provided operations,
-- - a set of required operations,
-- - a mapping from operations to (input) messages, and
-- - a partial mapping from operations to (output) messages.
data Signature = Signature
  { providedOperations :: [Operation]
  , requiredOperations :: [Operation]
  , input              :: Map Operation Message
  , output             :: Map Operation (Maybe Message)
  } deriving (Show, Generic)

{-|
Eq instance for signatures.

Eq is upto reordering in provided and required operations.
-}
instance Eq Signature where
  (Signature ps qs fin fout) == (Signature ps' qs' fin' fout') =
    fromList ps == fromList ps' &&
    fromList qs == fromList qs' &&
    fin == fin' &&
    fout == fout'

{-|
FromJSON instance for signatures.
-}
instance FromJSON Signature

{-|
ToJSON instance for signatures.
-}
instance ToJSON Signature

-- |A time constraint is used to specify a minimum and maximum time interval
-- between two events (a start event and an end event).
data TimeConstraint = TimeConstraint
  { startEvent :: VEvent
  , stopEvent  :: VEvent
  , beginTime  :: Natural
  , endTime    :: Natural
  } deriving (Eq, Ord, Show, Generic)

-- |Hash for time constraints.
instance Hashable TimeConstraint where
  hashWithSalt s (TimeConstraint e1 e2 d1 d2) =
    s `hashWithSalt` e1 `hashWithSalt` e2 `hashWithSalt` d1 `hashWithSalt` d2

{-|
FromJSON instance for time constraints.
-}
instance FromJSON TimeConstraint

{-|
ToJSON instance for time constraints.
-}
instance ToJSON TimeConstraint

-- |A join point is a name (possibly Self) and an operation.
data JoinPoint = JoinPoint
  { name      :: VName
  , operation :: Operation
  } deriving (Eq, Ord, Generic)

-- |Show instance for join points.
instance Show JoinPoint where
  show (JoinPoint n o) = show o <> "#" <> show n

{-|
FromJSON instance for join points.
-}
instance FromJSON JoinPoint

{-|
ToJSON instance for join points.
-}
instance ToJSON JoinPoint

-- | A Marker to indicates whether a binding is internal or external
data BindingType = Internal | External
  deriving (Eq, Ord, Generic)

-- |Show instance for binding type.
instance Show BindingType where
  show Internal = "-->"
  show External = "==>"

{-|
FromJSON instance for binding types.
-}
instance FromJSON BindingType

{-|
ToJSON instance for binding types.
-}
instance ToJSON BindingType

-- |A binding relates two operations in two components.
-- It can be internal or external.
data Binding
  = Binding { bindingType :: BindingType
            , bindingId   :: VName
            , from        :: JoinPoint
            , to          :: JoinPoint }
  deriving (Eq, Ord, Generic)

-- |Show instance for bindings.
instance Show Binding where
  show (Binding bType i j1 j2) =
    "@" <> show i <> ": " <> show j1 <> show bType <> show j2

{-|
FromJSON instance for bindings.
-}
instance FromJSON Binding

{-|
ToJSON instance for bindings.
-}
instance ToJSON Binding

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
-- TODO: checking
data Component
  = BasicComponent { componentId     :: VName
                   , signature       :: Signature
                   , behavior        :: VLTS
                   , timeconstraints :: [TimeConstraint] }
  | CompositeComponent { componentId :: VName
                       , signature   :: Signature
                       , children    :: [ComponentInstance]
                       , inbinds     :: [Binding]
                       , extbinds    :: [Binding] }
  deriving (Eq,Show,Generic)

{-|
FromJSON instance for components.
-}
instance FromJSON Component

{-|
ToJSON instance for components.
-}
instance ToJSON Component

{-|
Component instance.

A component instance is an instance name and a component (type).
-}
data ComponentInstance
  = ComponentInstance { instanceId    :: VName
                      , componentType :: Component }
  deriving (Eq,Show,Generic)

{-|
FromJSON instance for component instances.
-}
instance FromJSON ComponentInstance

{-|
ToJSON instance for component instances.
-}
instance ToJSON ComponentInstance

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
  where
    os = S.fromList $ ps <> rs
    xs `elem'` x = x `elem` xs

-- |Check the validity of a behavior with reference to a signature.
-- A behavior is valid with reference to a signature iff:
-- - it is valid in the sense of LTS, and
-- - TODO: the alphabet is the smallest set such that ...
isValidBehavior :: Signature -> VLTS -> Bool
isValidBehavior _ = isValidLTS

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
-- TODO: A composite component is valid iff ...
-- (this should include the hypotheses for flatten)
isValidComponent :: Component -> Bool
isValidComponent (BasicComponent i s b tcs) =
  cond0 && cond1 && cond2 && cond3 && cond4
  where
    cond0 = isValidName i
    cond1 = isValidSignature s
    cond2 = isValidBehavior s b
    cond3 = getAll $ foldMap (All . isValidTimeConstraint b) tcs
    cond4 = null tcs || (not . hasLoop) b
isValidComponent CompositeComponent{} = True
