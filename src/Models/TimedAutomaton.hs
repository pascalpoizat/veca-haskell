-----------------------------------------------------------------------------
-- |
-- Module      :  Models.TimedAutomaton
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A type for Timed Automaton (TA).
-----------------------------------------------------------------------------

module Models.TimedAutomaton (
    -- * constructors
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
  , toXta)
where

import           Data.Foldable               (elem)
import           Data.Map                    (Map)
import           Data.Monoid                 ((<>))
import           Helpers                     (allIn)
import           Transformations.ModelToText (foldMapToString)

-- |A clock. This is the encapsulation of a String.
newtype Clock
  = Clock String
  deriving (Eq,Ord,Show)

-- |A location.
newtype Location a
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
  Edge {source :: Location b        -- ^ source location
       ,action :: a                 -- ^ action
       ,guard  :: [ClockConstraint] -- ^ guard
       ,resets :: [Clock]           -- ^ set of clocks to reset
       ,target :: Location b        -- ^ target location
       }
  deriving (Eq,Ord,Show)

-- |A timed automaton.
data TimedAutomaton a b =
  TimedAutomaton {modelId         :: String                             -- ^ id of the model
                 ,locations       :: [Location b]                       -- ^ locations
                 ,initialLocation :: Location b                         -- ^ initial location
                 ,clocks          :: [Clock]                            -- ^ clocks
                 ,actions         :: [a]                                -- ^ actions
                 ,edges           :: [Edge a b]                         -- ^ edges
                 ,invariants      :: Map (Location b) [ClockConstraint] -- ^ invariants
                 }
  deriving (Show)

-- |Alias for 'TimedAutomaton'
type TA = TimedAutomaton

-- |Check the validity of a 'TimedAutomaton'.
-- An 'TimedAutomaton' is valid iff:
--
-- - the model id is not empty
-- - the set of actions is not empty
-- - the set of locations is not empty
-- - the initial location is in the set of locations
-- - the source location of each edge is in the set of locations
-- - the label of each transition is in the alphabet
-- - the target location of each edge is in the set of locations
-- - the resets of each edge are in the set of clocks
isValidTA :: (Eq a, Eq b) => TimedAutomaton a b -> Bool
isValidTA (TimedAutomaton i ls l0 cs as es is)
  | length i == 0 = False
  | null as = False
  | null ls = False
  | not (l0 `elem` ls) = False
  | not $ (fmap source es) `allIn` ls = False
  | not $ (fmap action es) `allIn` as = False
  | not $ (fmap target es) `allIn` ls = False
  | not $ (foldMap resets es) `allIn` cs = False
  | otherwise = True

-- |Transform a 'TimedAutomaton' into the XTA format
toXta :: (Show a, Show b) => TimedAutomaton a b -> String
toXta (TimedAutomaton i ls l0 cs as es is) =
  unlines [sclocks
          ,schannels
          ,sheader
          ,sstates
          ,sinitialization
          ,stransitions
          ,sfooter
          ,sinstances
          ,sprocess]
  where sclocks = foldMapToString "clock " ", " ";" clock2string cs
        schannels = foldMapToString "chan " ", " ";" alphabet2string as
        sheader = "process " <> i <> "(){"
        sstates = foldMapToString "state " ", " ";" location2string ls
        sinitialization = "init " <> (location2string l0) <> ";"
        stransitions = ""
        sfooter = "}"
        sinstances = "Process = " <> i <> "();"
        sprocess = "system Process;"
        --
        clockPrefix = "c_"
        locationPrefix = "l_"
        clock2string (Clock c) = clockPrefix <> c
        alphabet2string = show
        location2string (Location a) = locationPrefix <> show a
