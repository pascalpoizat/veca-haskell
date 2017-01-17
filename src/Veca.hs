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

module Veca (-- * constructors
              Message
            , Operation
            , Signature
            , Behavior
            , TimeConstraint
            -- * helpers to construct values
            , signature
            , behavior
            , timeconstraint
             )
where

import           Control.Monad            (mfilter)
import           Data.Map                 as M (Map, keysSet)
import           Data.Set                 as S (Set, intersection,
                                                member, null, union)
import           LabelledTransitionSystem as L
import           Numeric.Natural

-- |A message. This is simply a String.
type Message = String

-- |An operation. This is simply a String.
type Operation = String

-- |A signature.
data Signature =
  Signature {providedOperations :: Set Operation                   -- ^ set of the provided operations
            ,requiredOperations :: Set Operation                   -- ^ set of the required operations
            ,input              :: (Map Operation Message)         -- ^ input messages of operations
            ,output             :: (Map Operation (Maybe Message)) -- ^ output messages of operations
            }
  deriving Show

-- |A behavior is a 'CIOLTS' defined over 'Operation's
type Behavior = CIOLTS Operation

-- |A time constraint is used to specify a minimum and maximum time interval between two events
data TimeConstraint =
  TimeConstraint{beginEvent :: (CIOEvent Operation) -- ^ first event
                ,endEvent   :: (CIOEvent Operation) -- ^ second event
                ,beginTime  :: Natural              -- ^ minimum time interval
                ,endTime    :: Natural              -- ^ maximum time interval
                }
  deriving Show

-- |Build a 'Signature'
--
-- If arguments are valid, return 'Just' the 'Signature' else 'Nothing'.
--
-- A 'Signature' is valid iff:
--
-- - the sets of provided and required operations are disjoint
-- - the domain of input is the set of operations (provided and required)
-- - the domain of output is the set of operations (provided and required)
signature :: Set Operation
          -> Set Operation
          -> Map Operation Message
          -> Map Operation (Maybe Message)
          -> Maybe Signature
signature ps rs fi fo
  | not $ S.null (ps `S.intersection` rs) = Nothing
  | M.keysSet fi /= os || M.keysSet fo /= os = Nothing
  | otherwise = Just $ Signature ps rs fi fo
  where os = ps `S.union` rs

-- |Build a 'Behavior'
--
-- If arguments are valid, return 'Just' the 'Behavior' else 'Nothing'.
--
-- A 'Behavior' is valid with reference to a 'Signature' iff:
--
-- - it is valid in the sense of 'LTS'
-- - the alphabet is the smallest set such that: TODO
behavior :: Signature
         -> Set (CIOEvent Operation)
         -> Set State
         -> State
         -> Set State
         -> Set (Transition (CIOEvent Operation))
         -> Maybe Behavior
behavior (Signature ps rs fi fo) as ss i fs ts
  = mfilter isValid . pure $ LTS as ss i fs ts -- TODO use a validation constructor in LTS too

-- |Build a 'TimeConstraint'.
--
-- If arguments are valid, return 'Just' the 'TimeConstraint' else 'Nothing'.
--
-- A 'TimeConstraint' is valid with reference to a 'Behavior' b iff:
--
-- - beginTime < endTime
-- - beginEvent and endEvent are in the alphabet of b
timeconstraint :: Behavior
               -> CIOEvent Operation
               -> CIOEvent Operation
               -> Natural
               -> Natural
               -> Maybe TimeConstraint
timeconstraint (LTS as _ _ _ _) a1 a2 t1 t2
  | t1 >= t2 = Nothing
  | not $ a1 `S.member` as = Nothing
  | not $ a2 `S.member` as = Nothing
  | otherwise = Just $ TimeConstraint a1 a2 t1 t2
