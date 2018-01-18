{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Models.Events
Description : A module for communication events.
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Models.Events (
    -- * constructors
    IOEvent(..)
  , IOLTS
  , CIOEvent(..)
  , CIOLTS)
where

import           Data.Aeson
import           Data.Hashable                   (Hashable)
import           GHC.Generics                    (Generic)
import           Models.Communication            (Communication (..))
import           Models.Complementary            (Complementary (..))
import           Models.Internal                 (Internal (..))
import           Models.LabelledTransitionSystem (LTS)

{-|
Input-Output Events (IOEvents).
-}
data IOEvent a
  = Tau -- ^ internal action (non-observable)
  | Receive a -- ^ reception of something
  | Send a -- ^ sending of something
  deriving (Eq, Ord, Generic)

{-|
Hash instance for input-output events.
-}
instance Hashable a => Hashable (IOEvent a)

{-|
FromJSON instance for input-output events.
-}
instance (FromJSON a) => FromJSON (IOEvent a )

{-|
ToJSON instance for input-output events.
-}
instance (ToJSON a) => ToJSON (IOEvent a)

{-|
Communication input-output events (CIOEvents).
-}
data CIOEvent a
  = CTau -- ^ internal action (non-observable)
  | CReceive a -- ^ reception of a call
  | CReply a -- ^ reply to a call
  | CInvoke a -- ^ passing a call (= invocation)
  | CResult a -- ^ getting the result of a call
  deriving (Eq, Ord, Generic)

{-|
Hash instance for communication input-output events.
-}
instance Hashable a => Hashable (CIOEvent a)

{-|
FromJSON instance for communication input-output events.
-}
instance (FromJSON a) => FromJSON (CIOEvent a )

{-|
ToJSON instance for communication input-output events.
-}
instance (ToJSON a) => ToJSON (CIOEvent a)

{-|
Input-output LTS (IOLTS).

This is an LTS where labels are input-output events.
-}
type IOLTS a = LTS (IOEvent a)

{-|
Communication input-output LTS (CIOLTS).

This is an LTS where labels are communication input-output events.
-}
type CIOLTS a = LTS (CIOEvent a)

{-|
Show instance for input-output events.
-}
instance Show a => Show (IOEvent a) where
  show Tau         = "tau"
  show (Receive a) = "receive " ++ show a
  show (Send a)    = "send " ++ show a

{-|
Complementary instance for input-output events.
-}
instance Complementary (IOEvent a) where
  complementary Tau         = Tau
  complementary (Receive a) = Send a
  complementary (Send a)    = Receive a

{-|
Internal instance for input-output events.
-}
instance Internal (IOEvent a) where
  isInternal Tau = True
  isInternal _   = False

{-|
Communication instance for input-output events.
-}
instance Communication (IOEvent a) where
  isInput (Receive _) = True
  isInput _           = False

{-|
Show instance for communication input-output events.
-}
instance Show a => Show (CIOEvent a) where
  show CTau         = "tau"
  show (CReceive a) = "receive " ++ show a
  show (CReply a)   = "reply " ++ show a
  show (CInvoke a)  = "invoke " ++ show a
  show (CResult a)  = "result " ++ show a

{-|
Complementary instance for communication input-output events.
-}
instance Complementary (CIOEvent a) where
  complementary CTau         = CTau
  complementary (CReceive a) = CInvoke a
  complementary (CReply a)   = CResult a
  complementary (CInvoke a)  = CReceive a
  complementary (CResult a)  = CReply a

{-|
Internal instance for communication input-output events.
-}
instance Internal (CIOEvent a) where
  isInternal CTau = True
  isInternal _    = False

{-|
Communication instance for communication input-output events.
-}
instance Communication (CIOEvent a) where
  isInput (CReceive _) = True
  isInput (CResult _)  = True
  isInput _            = False
