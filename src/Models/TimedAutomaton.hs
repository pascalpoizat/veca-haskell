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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.TimedAutomaton (
    -- * constructors
    Clock(..)
  , Location(..)
  , ClockOperator(..)
  , ClockConstraint(..)
  , Edge(..)
  , TimedAutomaton(..)
  , TA
  , ToXta
    -- * validity checking
  , isValidTA
    -- * model to text transformations
  , asXta)
where

import           Data.Foldable                   (elem)
import           Data.Map                        (Map)
import           Data.Monoid                     ((<>))
import           Helpers                         (allIn)
import           Transformations.ModelToText     (foldMapToString)
import           Models.LabelledTransitionSystem (IOEvent, CIOEvent)

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

-- |Typeclass for what can be exported in the XTA format.
class Show t => ToXta t where
  -- |Transform the typeclass into a String in the XTA format.
  asXta :: t -> String
  {-# MINIMAL asXta#-}

-- |ToXta instance for String.
instance ToXta String where
  asXta = id

-- |ToXta instance for Clock.
instance ToXta Clock where
  asXta (Clock c) = "c_" ++ (asXta c)

-- |ToXta instance for Location.
instance (ToXta a) => ToXta (Location a) where
  asXta (Location l) = "l_" ++ (asXta l)

-- |ToXta instance for IOEvent.
instance (ToXta a) => ToXta (IOEvent a) where
  asXta = show

-- |ToXta instance for CIOEvent.
instance (ToXta a) => ToXta (CIOEvent a) where
  asXta = show

-- |ToXta instance for Edge.
instance (ToXta a
         ,ToXta b) =>
         ToXta (Edge a b) where
  asXta (Edge s a gs rs s') =
    (replicate 4 ' ') ++ (asXta s) ++ " -> " ++ (asXta s') ++ " { " ++ gardes ++ synchros ++ assigns ++ "}"

      where gardes  = (foldMapToString " guard " ", " ";" asXta gs)
            synchros = "sync "  ++ (asXta a) ++ "; "
            assigns  = varBool ++ resets ++ "; "
            varBool  = "assign " ++ (asXta a) ++ "=true"
            resets  = (foldMapToString ", " " = 0 , " " = 0 " asXta rs)

-- / Function to show operators
affopr :: String -> String
affopr "LT" = "<"
affopr "GT" = ">"
affopr "LE" = "<="
affopr "GE" = ">="
affopr "EQ" = "<"


-- |ToXta instance for ClockConstraint.
instance ToXta ClockConstraint where
  asXta (ClockConstraint ll mm nn) = (asXta ll) ++ " " ++ affopr(show mm) ++ " " ++ show nn



-- |ToXta instance for TimedAutomaton.
--
-- Can be used to transform a TimedAutomaton into the XTA format
instance (ToXta a
         ,ToXta b) =>
         ToXta (TimedAutomaton a b) where
  asXta (TimedAutomaton i ls l0 cs as es is) =
    unlines $
    filter (not . null)
           [schannels
           ,sheader
           ,svarbool
           ,sclocks
           ,sstates
           ,sinitialization
           ,sedges
           ,sfooter
           ,sinstances
           ,sprocess]
    where schannels = foldMapToString "chan " ", " ";" asXta as
          sheader = "process " <> i <> "(){"
          svarbool= foldMapToString "bool " "=false, " "=false;" asXta as
          sclocks = foldMapToString "clock " ", " ";" asXta cs
          sstates = foldMapToString "state " ", " ";" asXta ls
          sinitialization = "init " <> (asXta l0) <> ";"
          sedges = foldMapToString "trans\n" ",\n" ";" asXta es
          sfooter = "}"
          sinstances = "Process = " <> i <> "();"
          sprocess = "system Process;"
