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
                      , TimedAutomaton(..)
                      , TA
                      -- * validity checking
                      , isValidTA
                      -- * model to text transformations
                      , toXta
                      )
where

import           Data.Map as M (Map)
import           Data.Set as S (Set, isSubsetOf, map, member, null)



-- |A clock. This is the encapsulation of a String.
data Clock
  = Clock String
  deriving (Eq,Ord,Show)

-- |A location.
data Location a
  = Location a
  deriving (Eq,Ord,Show)

-- |A clock comparison operator.
data ClockOperator
  = LT
  | GT
  | LE
  | GE
  | EQ
  deriving (Eq,Ord,Show)

-- |A clock constraint.
data ClockConstraint =
  ClockConstraint {clock    :: Clock         -- ^ clock
                  ,operator :: ClockOperator -- ^ comparison operator
                  ,value    :: Int           -- ^ value to compare to
                  }
  deriving (Eq,Ord,Show)

-- |An edge with actions of type a.
data Edge a b =
  Edge {source :: Location b          -- ^ source location
       ,action :: a                   -- ^ action
       ,guard  :: Set ClockConstraint -- ^ guard
       ,resets :: Set Clock           -- ^ set of clocks to reset
       ,target :: Location b          -- ^ target location
       }
  deriving (Eq,Ord,Show)

-- |A timed automaton.
data TimedAutomaton a b =
  TimedAutomaton {locations       :: Set (Location b)                       -- ^ locations
                 ,initialLocation :: Location b                             -- ^ initial location
                 ,clocks          :: Set Clock                              -- ^ clocks
                 ,actions         :: Set a                                  -- ^ actions
                 ,edges           :: Set (Edge a b)                         -- ^ edges
                 ,invariants      :: Map (Location b) (Set ClockConstraint) -- ^ invariants
                 }
  deriving (Show)

-- |Alias for 'TimedAutomaton'
type TA = TimedAutomaton

-- |Check the validity of a 'TimedAutomaton'.
-- An 'TimedAutomaton' is valid iff:
--
-- - the set of actions is not empty
-- - the set of locations is not empty
-- - the initial location is in the set of locations
-- - the source location of each edge is in the set of locations
-- - the label of each transition is in the alphabet
-- - the target location of each edge is in the set of locations
-- - the resets of each edge are in the set of clocks <-- TODO
isValidTA :: (Ord a, Ord b) => TimedAutomaton a b -> Bool
isValidTA (TimedAutomaton ls l0 cs as es is)
  | S.null as = False
  | S.null ls = False
  | not (l0 `member` ls) = False
  | not (S.map source es `isSubsetOf` ls) = False
  | not (S.map action es `isSubsetOf` as) = False
  | not (S.map target es `isSubsetOf` ls) = False
  | otherwise = True

-- |Transform a 'TimedAutomaton' into the XTA format

toXta :: (Show a, Show b) =>TimedAutomaton a b -> String
toXta (TimedAutomaton ls l0 cs as es is) = enteteBloc ++ " " ++ clockBloc ++ " " ++ statesBloc ++ " " ++ initBloc ++ " " ++ transBloc ++ " " ++ enBloc -- ++ leTout

  where enteteBloc = "process name () {"
        clockBloc  = "clock" -- TODO
        statesBloc = "state" ++ " " ++ tail (takeWhile (/=']') (dropWhile (/='[') (show ls))) ++ ";"
        initBloc   = "init" ++ " " ++ show l0 ++ ";"
        transBloc  = "transBloc" -- TODO
        enBloc     = "}"
        -- leTout     = show (TimedAutomaton ls l0 cs as es is)
