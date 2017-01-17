-----------------------------------------------------------------------------
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

module Veca (-- * basic types
              Message
            , Operation
            , Name
            -- * instantiated types
            , BehaviorEvent
            , Behavior
            -- * constructors
            , Signature(..)
            , TimeConstraint(..)
            , Binding(..)
            , Component(..)
            -- * validity checking
            , isValidSignature
            , isValidBehavior
            , isValidTimeConstraint
            , isValidComponent
             )
where

import           Data.Map                 as M (Map, keys)
import           Data.Set                 as S (Set, filter, fromList,
                                                intersection, map, member, null,
                                                toList, union)
import           LabelledTransitionSystem as L

-- |A name. This is simply a String.
type Name = String

-- |A message. This is simply a String.
type Message = String

-- |An operation. This is simply a String.
type Operation = String

-- |A signature is given as a set of provided operations, a set of required operations,
-- a mapping from operations to (input) messages, and
-- a partial mapping from operations to (output) messages.
data Signature =
  Signature {providedOperations :: Set Operation                   -- ^ set of the provided operations
            ,requiredOperations :: Set Operation                   -- ^ set of the required operations
            ,input              :: (Map Operation Message)         -- ^ input messages of operations
            ,output             :: (Map Operation (Maybe Message)) -- ^ output messages of operations
            }
  deriving (Show)

-- |A behavior is a 'CIOLTS' defined over 'Operation's
type Behavior = CIOLTS Operation

-- |A communication event based on 'Operation's.
type BehaviorEvent = CIOEvent Operation

-- |A time constraint is used to specify a minimum and maximum time interval between two events
data TimeConstraint =
  TimeConstraint {startEvent :: BehaviorEvent -- ^ first event
                 ,stopEvent  :: BehaviorEvent -- ^ second event
                 ,beginTime  :: Int           -- ^ minimum time interval
                 ,endTime    :: Int           -- ^ maximum time interval
                 }
  deriving (Eq,Ord,Show)

-- |A binding relates two operations in two components. It can be internal or external.
data Binding
  = InternalBinding
  | ExternalBinding
  deriving (Show)

-- |A component is either a basic or a composite component.
-- A basic component is given as a signature, a behavior, and time constraints.
-- A composite component ... TODO
data Component
  = BasicComponent {signature       :: Signature          -- ^ signature
                   ,behavior        :: Behavior           -- ^ behavior
                   ,timeconstraints :: Set TimeConstraint -- ^ time constraints
                   }
  | CompositeComponent {signature :: Signature          -- ^ signature
                       ,children  :: Map Name Component -- ^ typed subcomponents
                       ,inbinds   :: Set Binding        -- ^ internal bindings
                       ,extbinds  :: Set Binding        -- ^ external bindings
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
  | S.fromList (M.keys fi) /= os || S.fromList (M.keys fo) /= os = False
  | otherwise = True
  where os = ps `S.union` rs

-- |Check the validity of a 'Behavior' with reference to a 'Signature'.
--
-- A 'Behavior' is valid with reference to a 'Signature' iff:
--
-- - it is valid in the sense of 'LTS'
-- - the alphabet is the smallest set such that: TODO
isValidBehavior :: Signature -> Behavior -> Bool
isValidBehavior s b@(LTS as ss i fs ts)
  | not $ isValidLTS b = False
  | otherwise = True -- TODO

-- |Check the validity of a 'TimeConstraint' with reference to a 'Behavior'.
--
-- A 'TimeConstraint' is valid with reference to a 'Behavior' b iff:
--
-- - beginTime >=0 and endTime >= 0
-- - beginTime < endTime
-- - beginEvent and endEvent are in the alphabet of b
isValidTimeConstraint :: Behavior -> TimeConstraint -> Bool
isValidTimeConstraint b (TimeConstraint a1 a2 t1 t2)
  | t1 < 0 || t2 < 0 || t1 >= t2 = False
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
--
-- A 'CompositeComponent' is valid iff: TODO
isValidComponent :: Component -> Bool
isValidComponent (BasicComponent s b tcs) =
  isValidSignature s &&
  isValidBehavior s b &&
  (foldr (&&) True $
   Prelude.map (isValidTimeConstraint b)
               (S.toList tcs))
isValidComponent (CompositeComponent s cs ibs ebs) = True -- TODO

-- |Get all 'Transition's whose label is the start event of a 'TimeConstraint'.
tcsource
  :: Behavior -> TimeConstraint -> Set BehaviorEvent
tcsource (LTS _ _ _ _ ts) (TimeConstraint a1 _ _ _) =
  S.filter (== a1) $ S.map label ts

-- |Get all 'Transition's whose label is the stop event of a 'TimeConstraint'.
tctarget
  :: Behavior -> TimeConstraint -> Set BehaviorEvent
tctarget (LTS _ _ _ _ ts) (TimeConstraint _ a2 _ _) =
  S.filter (== a2) $ S.map label ts
