{-|
Module      : Models.TimedAutomaton
Description : A (simplified) type for Timed Automata (TA) extended as in the UPPAAL tool (and XTA format).
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
  , VariableType(..)
  , VariableName(..)
  , VariableTyping(..)
  , Bounds(..)
  , Expression(..)
  , VariableAssignment(..)
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
    -- * get/set
  , isCommitted
  , isUrgent
  , setCommitted
  , setUrgent
    -- * relabelling
  , name
  , rename
  , prefixBy
  , suffixBy
  , relabel
  , rename'
    -- * model to text transformations
  , asXta)
where

import           Data.List                    (delete)
import           Data.Map                     (Map (..))
import           Data.Monoid                  (Any (..), getAny, (<>))
import           Data.Set                     (fromList)
import           Helpers                      (allIn, removeDuplicates)
import           Models.Communication         (Communication (..))
import           Models.Events                (CIOEvent (..), IOEvent (..))
import           Models.Internal              (Internal (..))
import           Models.Name                  (Name (..), isValidName)
import           Models.Named                 (Named (..))
import           Numeric.Natural
import           Transformations.ModelToText  (foldMapToString,
                                               foldMapToString')
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
A variable type, used for variables.
-}
data VariableType = IntType Bounds
                | BoolType
  deriving (Eq, Show)

{-|
Boundaries for the integer type.

This is a simplified version of the UPPAAL model (no constants).
-}
data Bounds = NoBounds | Bounds {lowerBound :: Int, higherBound :: Int}
  deriving (Eq, Show)

{-|
Variable names are names over String.
-}
type VariableName = Name String

{-|
Variables are given as a name, a type and possibly an initialization.
-}
data VariableTyping = VariableTyping {varname :: VariableName, vartype :: VariableType, varinit :: Maybe Expression}
  deriving (Eq, Show)

{-|
An expression used in assignments.

The expression is abstracted as a String.
We suppose this String is correct (type and use of variables).
-}
newtype Expression = Expression String
  deriving (Eq, Show)

{-|
An assignment for a variable.
-}
data VariableAssignment = VariableAssignment {variable :: VariableName, value :: Expression}

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
  { ccclock    :: Clock -- ^ clock
  , ccoperator :: ClockOperator -- ^ comparison operator
  , ccvalue    :: Natural -- ^ value to compare to
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
  { mid                :: Name String -- ^ id of the model
  , locations          :: [Location b] -- ^ locations
  , initialLocation    :: Location b -- ^ initial location
  , committedLocations :: [Location b] -- ^ committed locations
  , urgentLocations    :: [Location b] -- ^ urgent locations
  , clocks             :: [Clock] -- ^ clocks
  , variables          :: Map VariableName VariableTyping -- ^ variables
  , actions            :: [a] -- ^ actions
  , edges              :: [Edge a b] -- ^ edges
  , invariants         :: [(Location b, [ClockConstraint])] -- ^ invariants
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
  (TimedAutomaton i ls l0 cls uls cs vs as es _) == (TimedAutomaton i' ls' l0' cls' uls' cs' vs' as' es' _) =
    and
      [ i == i'
      , fromList ls == fromList ls'
      , l0 == l0'
      , cls == cls'
      , uls == uls'
      , fromList cs == fromList cs'
      , vs == vs'
      , fromList as == fromList as'
      , fromList es == fromList es'
      ]

{-|
Instance of Named for TAs.
-}
instance Named (TimedAutomaton a b) where
  name = mid
  rename n (TimedAutomaton _ ls l0 cls uls cs vs as es is) =
    TimedAutomaton n ls l0 cls uls cs vs as es is

{-|
Check the validity of a TA.

A TA is valid iff:
- the model id is not empty
- the set of actions is not empty
- the set of locations is not empty
- the sets of urgent and committed locations are disjoint
- the union of the urgent and committed locations is included in the locationd
- the initial location is in the set of locations
- the source location of each edge is in the set of locations
- the label of each transition is in the alphabet
- the target location of each edge is in the set of locations
- the resets of each edge are in the set of clocks
- TODO: the keyset of the invariants is equal to the set of locations
- TODO: variables used in edges correspond to the declared variables
-}
isValidTA :: (Eq a, Eq b) => TimedAutomaton a b -> Bool
isValidTA (TimedAutomaton i ls l0 cls uls cs vs as es _) = and
  [ isValidName i
  , not . null $ as
  , not . null $ ls
  , l0 `elem` ls
  , not . getAny $ foldMap (Any . elem' uls) cls
  , cls `allIn` ls
  , uls `allIn` ls
  , (source <$> es) `allIn` ls
  , (action <$> es) `allIn` as
  , (target <$> es) `allIn` ls
  , (rclock <$> foldMap resets es) `allIn` cs
  ]
  where
    xs `elem'` x = x `elem` xs

{-|
Relabel actions in a TA.
-}
relabel :: (Ord a) => Substitution a -> TimedAutomaton a b -> TimedAutomaton a b
relabel sigma (TimedAutomaton i ls l0 cls uls cs vs as es is) =
  TimedAutomaton i ls l0 cls uls cs vs (apply sigma <$> as) (relabelE sigma <$> es) is
  where
    relabelE sig (Edge s a gs rs s') = Edge s (apply sig a) gs rs s'

{-|
Check if a location is committed.
-}
isCommitted :: Eq b => TimedAutomaton a b -> Location b -> Bool
isCommitted (TimedAutomaton i ls l0 cls uls cs vs as es is) l =
  l `elem` ls && l `elem` cls

{-|
Check if a location is urgent.
-}
isUrgent :: Eq b => TimedAutomaton a b -> Location b -> Bool
isUrgent (TimedAutomaton i ls l0 cls uls cs vs as es is) l =
  l `elem` ls && l `elem` uls

{-|
Set a location to be committed.

Supposes the timed automaton is valid.
-}
setCommitted :: Eq b => TimedAutomaton a b -> Location b -> TimedAutomaton a b
setCommitted t@(TimedAutomaton i ls l0 cls uls cs vs as es is) l
  | l `notElem` ls     = t
  | isCommitted t l    = t
  | not (isUrgent t l) = TimedAutomaton i ls l0 cls' uls cs vs as es is
  | otherwise          = TimedAutomaton i ls l0 cls' uls' cs vs as es is
 where
  cls' = l : cls
  uls' = delete l uls

{-|
Set a location to be urgent.

Supposes the timed automaton is valid.
-}
setUrgent :: Eq b => TimedAutomaton a b -> Location b -> TimedAutomaton a b
setUrgent t@(TimedAutomaton i ls l0 cls uls cs vs as es is) l
  | l `notElem` ls        = t
  | isUrgent t l          = t
  | not (isCommitted t l) = TimedAutomaton i ls l0 cls uls' cs vs as es is
  | otherwise             = TimedAutomaton i ls l0 cls' uls' cs vs as es is
 where
  cls' = delete l cls
  uls' = l : uls

{-|
Rename the TA (using a substitution).
-}
rename' :: Substitution (Name String) -> TimedAutomaton a b -> TimedAutomaton a b
rename' sigma t = rename (apply sigma $ name t) t

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
instance ToXta (Name String) where
  asXta (Name []) = "_"
  asXta (Name ns) = foldMapToString' "_" id ns

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
ToXta instance for expressions.
-}
instance ToXta Expression where
  asXta (Expression e) = asXta e

{-|
ToXta instance for bounds.
-}
instance ToXta Bounds where
  asXta NoBounds       = ""
  asXta (Bounds b1 b2) = "[" ++ asXta b1 ++ "," ++ asXta b2 ++ "]"

{-|
ToXta instance for variable types.
-}
instance ToXta VariableType where
  asXta (IntType b) = "int" ++ asXta b
  asXta BoolType    = "bool"

{-|
ToXta instance for variable typings.
-}
instance ToXta VariableTyping where
  asXta (VariableTyping v t (Just e)) = asXta t ++ " " ++ asXta v ++ " = " ++ asXta e ++ ";"
  asXta (VariableTyping v t Nothing) = asXta t ++ " " ++ asXta v ++ ";"

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
  asXta (CReceive a) = asXta a ++ reqSuffix
  asXta (CInvoke a)  = asXta a ++ reqSuffix
  asXta (CReply a)   = asXta a ++ resSuffix
  asXta (CResult a)  = asXta a ++ resSuffix

reqSuffix :: String
reqSuffix = "_req"

resSuffix :: String
resSuffix = "_res"

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

Can be used to transform a TA into the XTA format.
Given a TA t, the channels and instance parts of the XTA files
are obtained by using @ToXta (TimedAutomataNetwork [t])@ instead of @ToXta t@.
-}
instance (Ord a, Ord b, ToXta a, ToXta b, Communication a) =>
         ToXta (TimedAutomaton a b) where
  asXta (TimedAutomaton i ls l0 cls uls cs vs as es is) =
    unlines $
    filter
      (not . null)
      [ sheader
      , sclocks
      , svariables
      , sstates
      , scstates
      , sustates
      , sinitialization
      , sedges
      , sfooter
      ]
    where
      sheader = "process " <> asXta i <> "(){"
      sclocks = foldMapToString "clock " ", " ";" asXta cs
      svariables = foldMapToString' "\n" asXta vs
      sstates = foldMapToString "state " ", " ";" (asXtaWithInvariants is) ls
      scstates = foldMapToString "commit " ", " ";" asXta cls
      sustates = foldMapToString "urgent " ", " ";" asXta uls
      sinitialization = "init " <> asXta l0 <> ";"
      sedges = foldMapToString "trans\n" ",\n" ";" asXta es
      sfooter = "}"
      asXtaWithInvariants is' l =
        asXta l <>
        foldMapToString " { " " && " " }" asXta (getInvariantForLocation is' l)
