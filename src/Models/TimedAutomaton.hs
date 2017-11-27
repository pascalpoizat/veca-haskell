{-|
Module      : Models.TimedAutomaton
Description : A type for Timed Automata (TA).
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
{-# LANGUAGE FlexibleInstances #-}

module Models.TimedAutomaton (
    -- * constructors
    Clock(..)
  , Location(..)
  , ClockOperator(..)
  , ClockConstraint(..)
  , ClockReset(..)
  , Edge(..)
  , TimedAutomaton(..)
  , TimedAutomataNetwork(..)
  , ToXta
    -- * validity checking
  , isValidTA
    -- * relabelling
  , relabel
    -- * model to text transformations
  , asXta)
where

import           Data.Monoid                  ((<>))
import           Data.Set                     (fromList)
import           Helpers                      (allIn, removeDuplicates)
import           Models.Communication         (Communication (..))
import           Models.Events                (CIOEvent (..), IOEvent (..))
import           Models.Internal              (Internal (..))
import           Models.Name                  (Name (..), isValidName)
import           Numeric.Natural
import           Transformations.ModelToText  (foldMapToString)
import           Transformations.Substitution (Substitution, apply)

{-|
A clock.
-}
newtype Clock =
  Clock String
  deriving (Eq, Ord, Show)

{-|
A location.
-}
newtype Location b =
  Location b
  deriving (Eq, Ord, Show)

{-|
A clock comparison operator.
-}
data ClockOperator
  = LT
  | GT
  | LE
  | GE
  | EQ
  deriving (Eq, Ord, Show)

{-|
A clock constraint.
-}
data ClockConstraint = ClockConstraint
  { cclock   :: Clock -- ^ clock
  , operator :: ClockOperator -- ^ comparison operator
  , value    :: Natural -- ^ value to compare to
  } deriving (Eq, Ord, Show)

{-|
A clock reset (resets a clock to 0).
-}
newtype ClockReset = ClockReset
  { rclock :: Clock
  } deriving (Eq, Ord, Show)

{-|
An edge with actions of type a between locations of type b.
-}
data Edge a b = Edge
  { source :: Location b -- ^ source location
  , action :: a -- ^ action
  , guard  :: [ClockConstraint] -- ^ guard
  , resets :: [ClockReset] -- ^ set of clocks to reset
  , target :: Location b -- ^ target location
  } deriving (Ord, Show)

{-|
Instance of Eq for edges.

Two edges are == up tp reordering in guard and resets.
-}
instance (Eq a, Eq b) => Eq (Edge a b) where
  (Edge s a gs rs t) == (Edge s' a' gs' rs' t') =
    (s == s') &&
    (a == a') &&
    (fromList gs == fromList gs') && (fromList rs == fromList rs') && (t == t')

{-|
A timed automaton (TA).

A TA is generic on a, the the type of actions on edges,
and on b, the type of locations.
-}
data TimedAutomaton a b = TimedAutomaton
  { mid             :: Name -- ^ id of the model
  , locations       :: [Location b] -- ^ locations
  , initialLocation :: Location b -- ^ initial location
  , clocks          :: [Clock] -- ^ clocks
  , actions         :: [a] -- ^ actions
  , edges           :: [Edge a b] -- ^ edges
  , invariants      :: [(Location b, [ClockConstraint])] -- ^ invariants
  }

{-|
Network of TAs.
-}
newtype TimedAutomataNetwork a b =
  TimedAutomataNetwork [TimedAutomaton a b]
  deriving (Show)

{-|
Instance of Show for TAs.
-}
instance (Ord a, Ord b, ToXta a, ToXta b, Communication a) =>
         Show (TimedAutomaton a b) where
  show = asXta

{-|
Instance of Eq for TAs.

Two TAs are == upto reordering in collections (locations, clocks, edges, invariants).

TODO: add treatment for invariants
-}
instance (Ord a, Ord b) => Eq (TimedAutomaton a b) where
  (TimedAutomaton i ls l0 cs as es _) == (TimedAutomaton i' ls' l0' cs' as' es' _) =
    (i == i') &&
    (fromList ls == fromList ls') &&
    (l0 == l0') &&
    (fromList cs == fromList cs') &&
    (fromList as == fromList as') && (fromList es == fromList es') && True

{-|
Check the validity of a TA.

A TA is valid iff:
- the model id is not empty
- the set of actions is not empty
- the set of locations is not empty
- the initial location is in the set of locations
- the source location of each edge is in the set of locations
- the label of each transition is in the alphabet
- the target location of each edge is in the set of locations
- the resets of each edge are in the set of clocks
- TODO: the keyset of the invariants is equal to the set of locations
-}
isValidTA :: (Eq a, Eq b) => TimedAutomaton a b -> Bool
isValidTA (TimedAutomaton i ls l0 cs as es _)
  | not . isValidName $ i = False
  | null as = False
  | null ls = False
  | l0 `notElem` ls = False
  | not $ (source <$> es) `allIn` ls = False
  | not $ (action <$> es) `allIn` as = False
  | not $ (target <$> es) `allIn` ls = False
  | not $ (rclock <$> foldMap resets es) `allIn` cs = False
  | otherwise = True

{-|
Relabel actions in a TA.
-}
relabel :: (Ord a) => Substitution a -> TimedAutomaton a c -> TimedAutomaton a c
relabel sigma (TimedAutomaton i ls l0 cs as es is) =
  TimedAutomaton i ls l0 cs (apply sigma <$> as) (relabelE sigma <$> es) is
  where
    relabelE sig (Edge s a gs rs s') = Edge s (apply sig a) gs rs s'

{-|
Get the invariant for a location.
-}
getInvariantForLocation ::
     Ord b
  => [(Location b, [ClockConstraint])]
  -> Location b
  -> [ClockConstraint]
getInvariantForLocation is l = foldMap snd . filter ((== l) . fst) $ is

{-|
Class for what can be exported in the XTA format.
-}
class Show t =>
      ToXta t
      where
  asXta :: t -> String
  {-# MINIMAL asXta #-}

{-|
Symbol in the XTA format for reception.
-}
xtaREC :: String
xtaREC = "?"

{-|
Symbol in the XTA format for emission.
-}
xtaSEND :: String
xtaSEND = "!"

{-|
ToXta instance for names.
-}
instance ToXta Name where
  asXta = show

{-|
ToXta instance for Natural.
-}
instance ToXta Natural where
  asXta = show

{-|
ToXta instance for Int.
-}
instance ToXta Int where
  asXta = show

{-|
ToXta instance for [Char].
-}
instance ToXta [Char] where
  asXta = id

{-|
ToXta instance for clocks.
-}
instance ToXta Clock where
  asXta (Clock c) = "c_" ++ asXta c

{-|
ToXta instance for clock resets.
-}
instance ToXta ClockReset where
  asXta (ClockReset c) = asXta c ++ " = 0"

{-|
ToXta instance for clock constraints.
-}
instance ToXta ClockConstraint where
  asXta (ClockConstraint c op v) = asXta c <> " " <> asXta op <> " " <> asXta v

{-|
ToXta instance for clock operators.
-}
instance ToXta ClockOperator where
  asXta Models.TimedAutomaton.LT = "<"
  asXta Models.TimedAutomaton.LE = "<="
  asXta Models.TimedAutomaton.EQ = "=="
  asXta Models.TimedAutomaton.GE = ">="
  asXta Models.TimedAutomaton.GT = ">"

{-|
ToXta instance for locations.
-}
instance (ToXta a) => ToXta (Location a) where
  asXta (Location l) = "l_" ++ asXta l

{-|
ToXta instance for IO events.
-}
instance (ToXta a) => ToXta (IOEvent a) where
  asXta Tau         = ""
  asXta (Receive a) = asXta a
  asXta (Send a)    = asXta a

{-|
ToXta instance for CIO events.
-}
instance (ToXta a) => ToXta (CIOEvent a) where
  asXta CTau         = ""
  asXta (CReceive a) = asXta a
  asXta (CInvoke a)  = asXta a
  asXta (CReply a)   = asXta a
  asXta (CResult a)  = asXta a

{-|
ToXta instance for edges.
-}
instance (ToXta a, ToXta b, Communication a) => ToXta (Edge a b) where
  asXta (Edge s a gs rs s') =
    concat
      [ replicate 4 ' '
      , asXta s
      , " -> "
      , asXta s'
      , " { "
      , foldMapToString "guard " " && " "; " asXta gs
      , asXta' a
      , foldMapToString "assign " ", " "; " asXta rs
      , "}"
      ]
    where
      asXta' e =
        case () of
          _
            | isOutput e -> "sync " ++ asXta e ++ xtaSEND ++ "; "
            | isInput e -> "sync " ++ asXta e ++ xtaREC ++ "; "
            | otherwise -> ""

{-|
ToXta instance for a TA network.
-}
instance (Ord a, Ord b, ToXta a, ToXta b, Communication a) =>
         ToXta (TimedAutomataNetwork a b) where
  asXta (TimedAutomataNetwork tas) =
    unlines $ [schannels] <> stas <> sinstances <> [sprocess]
    where
      -- define the channels
      schannels = foldMapToString "chan " ", " ";" id iochannels
      iochannels =
        removeDuplicates $ asXta <$> foldMap (removeInternals . actions) tas
      removeInternals = filter (not . isInternal)
      -- define the TAs
      stas = asXta <$> tas
      -- get all TA ids
      pids = mid <$> tas
      -- create an instance for each TA
      sinstances = finstancedecl <$> pids
      finstancedecl pid = finstancename pid <> " = " <> asXta pid <> "();"
      finstancename pid = "Process_" <> asXta pid
      -- put all the instances in the system
      sprocess = foldMapToString "system " ", " ";" finstancename pids

{-|
ToXta instance for TAs.

Can be used to transform a TA into the XTA format
-}
instance (Ord a, Ord b, ToXta a, ToXta b, Communication a) =>
         ToXta (TimedAutomaton a b) where
  asXta (TimedAutomaton i ls l0 cs as es is) =
    unlines $
    filter
      (not . null)
      [ schannels
      , sheader
      , sclocks
      , sstates
      , sinitialization
      , sedges
      , sfooter
      , sinstances
      , sprocess
      ]
    where
      schannels =
        foldMapToString "chan " ", " ";" asXta $ removeDuplicates iochannels
      sheader = "process " <> asXta i <> "(){"
      sclocks = foldMapToString "clock " ", " ";" asXta cs
      sstates = foldMapToString "state " ", " ";" (asXtaWithInvariants is) ls
      sinitialization = "init " <> asXta l0 <> ";"
      sedges = foldMapToString "trans\n" ",\n" ";" asXta es
      sfooter = "}"
      sinstances = "Process = " <> asXta i <> "();"
      sprocess = "system Process;"
      iochannels = asXta <$> ioevents
      ioevents = filter (not . isInternal) as
      asXtaWithInvariants is' l =
        asXta l <>
        foldMapToString " { " " && " " }" asXta (getInvariantForLocation is' l)
