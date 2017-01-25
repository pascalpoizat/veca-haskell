-----------------------------------------------------------------------------
-- |
-- Module      :  TimedAutomaton
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A type for Timed Automaton (TA).
-----------------------------------------------------------------------------

module TimedAutomaton (-- * constructors
                        Clock(..)
                      , Location(..)
                      , ClockOperator(..)
                      , ClockConstraint(..)
                      , Edge(..)
                      , TA(..)
                      -- * validity checking
                      , isValidTA
                      )
where

import           Data.Map as M (Map)
import           Data.Set as S (Set, isSubsetOf, map, member, null)

-- |A clock. This is the encapsulation of a String.
data Clock
  = Clock String
  deriving (Show)

-- |A location. This is the encapsulation of a String.
data Location
  = Location String
  deriving (Eq,Ord,Show)

-- |A clock comparison operator. Can be "<" (LT), ">" (GT), "<=" (LE), ">=" (GE), or "==" (EQ).
data ClockOperator
  = LT
  | GT
  | LE
  | GE
  | EQ
  deriving (Show)

-- |A clock constraint.
data ClockConstraint =
  ClockConstraint {clock    :: Clock         -- ^ clock of the 'ClockConstraint'
                  ,operator :: ClockOperator -- ^ clock comparison operator
                  ,value    :: Int           -- ^ value to compare to
                  }
  deriving (Show)

-- |An edge with actions of type a.
data Edge a =
  Edge {source :: Location            -- ^ source location of the 'Edge'
       ,action :: a                   -- ^ action of the 'Edge'
       ,guard  :: Set ClockConstraint -- ^ guard of the 'Edge'
       ,resets :: Set Clock           -- ^ set of clocks to reset of the 'Edge'
       ,target :: Location            -- ^ target location of the 'Edge'
       }
  deriving (Show)

-- |A timed automaton.
data TA a =
  TA {locations       :: Set Location                       -- ^ locations
     ,initialLocation :: Location                           -- ^ initial location
     ,clocks          :: Set Clock                          -- ^ clocks
     ,actions         :: Set a                              -- ^ actions
     ,edges           :: Set (Edge a)                       -- ^ edges
     ,invariants      :: Map Location (Set ClockConstraint) -- ^ invariants
     }
  deriving (Show)

-- |Check the validity of a 'TA'.
-- An 'TA' is valid iff:
--
-- - the set of actions is not empty
-- - the set of locations is not empty
-- - the initial location is in the set of locations
-- - the source location of each edge is in the set of locations
-- - the label of each transition is in the alphabet
-- - the target location of each edge is in the set of locations
-- - the resets of each edge are in the set of clocks
isValidTA :: (Ord a) => TA a -> Bool
isValidTA (TA ls l0 cs as es is) =
  not (S.null as) &&
  not (S.null ls) &&
  l0 `member` ls &&
  S.map source es `isSubsetOf` ls &&
  S.map target es `isSubsetOf` ls &&
  S.map action es `isSubsetOf` as &&
  True -- TODO

