-- main module for VECA
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
--
module Veca ( Component
            , Message
            , Operation
            , Signature(..)
            , isValidSignature
            , Behavior(..)
            , isValidBehavior
            )
where

import           Data.Map                 as M (Map)
import           Data.Set                 as S (Set)
import           LabelledTransitionSystem as L

-- basic types
type Component = String
type Message = String
type Operation = String

-- signatures
data Signature =
  Signature {providedOperations :: Set Operation
            ,requiredOperations :: Set Operation
            ,input              :: (Map Operation Message)
            ,output             :: (Map Operation (Maybe Message))}
  deriving Show

isValidSignature :: Signature -> Bool
isValidSignature (Signature ps rs fi fo) = True -- TODO

-- behaviors
type Behavior = CIOLTS Operation

isValidBehavior :: Signature -> Behavior -> Bool
isValidBehavior (Signature ps rs fi fo) (LTS as ss i fs ts) = True -- TODO

