-----------------------------------------------------------------------------
-- |
-- Module      :  Models.Events
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A module for communication events.
-----------------------------------------------------------------------------

module Models.Events (
    -- * constructors
    IOEvent(..)
  , IOLTS
  , CIOEvent(..)
  , CIOLTS)
where

import           Models.Communication            (Communication (..))
import           Models.Complementary            (Complementary (..))
import           Models.Internal                 (Internal (..))
import           Models.LabelledTransitionSystem (LTS)

-- |Input-Output Events (IOEvents).
-- Used as labels in 'IOLTS's.
data IOEvent a
  = Tau       -- ^ internal action (non-observable)
  | Receive a -- ^ reception of something
  | Send a    -- ^ sending of something
  deriving (Eq,Ord)

-- |Communication-Input-Output Events (CIOEvents).
-- Used as labels in 'CIOLTS's.
data CIOEvent a
  = CTau       -- ^ internal action (non-observable)
  | CReceive a -- ^ reception of a call
  | CReply a   -- ^ reply to a call
  | CInvoke a  -- ^ passing a call (= invocation)
  | CResult a  -- ^ getting the result of a call
  deriving (Eq,Ord)

-- |An Input-Output LTS (IOLTS).
-- This is an 'LTS' where labels are of type 'IOEvent'.
type IOLTS a = LTS (IOEvent a)

-- |Communication-Input-Output LTS (CIOLTS).
-- This is an 'LTS' where labels are of type 'CIOEvent'.
type CIOLTS a = LTS (CIOEvent a)

-- |Instance of Show for CIOEvent.
instance Show a =>
         Show (IOEvent a) where
  show Tau         = "tau"
  show (Receive a) = "receive " ++ (show a)
  show (Send a)    = "send " ++ (show a)

-- |Complementary for a 'IOEvent'.
instance Complementary (IOEvent a) where
  complementary Tau         = Tau
  complementary (Receive a) = Send a
  complementary (Send a)    = Receive a

-- |Instance of Internal for IOEvent.
instance Internal (IOEvent a) where
  isInternal Tau = True
  isInternal _   = False

-- |Instance of Communication for IOEvent.
instance Communication (IOEvent a) where
  isInput (Receive a) = True
  isInput _           = False

-- |Instance of Show for CIOEvent.
instance Show a =>
         Show (CIOEvent a) where
  show CTau         = "tau"
  show (CReceive a) = "receive " ++ (show a)
  show (CReply a)   = "reply " ++ (show a)
  show (CInvoke a)  = "invoke " ++ (show a)
  show (CResult a)  = "result " ++ (show a)

-- |Complementary for a 'IOEvent'.
instance Complementary (CIOEvent a) where
  complementary CTau         = CTau
  complementary (CReceive a) = CInvoke a
  complementary (CReply a)   = CResult a
  complementary (CInvoke a)  = CReceive a
  complementary (CResult a)  = CReply a

-- |Instance of Internal for CIOEvent.
instance Internal (CIOEvent a) where
  isInternal CTau = True
  isInternal _    = False

-- |Instance of Communication for CIOEvent.
instance Communication (CIOEvent a) where
  isInput (CReceive a) = True
  isInput (CResult a)  = True
  isInput _            = False
