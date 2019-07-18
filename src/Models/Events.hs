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
  , TIOEvent(..)
  , IOLTS
  , TIOLTS
  , CIOEvent(..)
  , CTIOEvent(..)
  , CIOLTS
  , CTIOLTS
  , liftToTIOEvent
  , liftToCTIOEvent)
where

import           Data.Aeson
import           Data.Hashable                   (Hashable)
import           GHC.Generics                    (Generic)
import           Models.Communication            (Communication (..))
import           Models.TCommunication           (TCommunication (..))
import           Models.Complementary            (Complementary (..))
import           Models.Internal                 (Internal (..))
import           Models.LabelledTransitionSystem (LTS)

{-|
Input-Output Events with internal events (TIOEvents).
-}
data TIOEvent a
  = TTau -- ^ internal action (non-observable)
  | TReceive a -- ^ reception of something
  | TSend a -- ^ sending of something
  deriving (Eq, Ord, Generic)

{-|
Input-Output Events (IOEvents).
-}
data IOEvent a
  = Receive a -- ^ reception of something
  | Send a -- ^ sending of something
  deriving (Eq, Ord, Generic)

liftToTIOEvent :: IOEvent a -> TIOEvent a
liftToTIOEvent (Receive a) = TReceive a
liftToTIOEvent (Send a) = TSend a

{-|
Hash instance for input-output events.
-}
instance Hashable a => Hashable (TIOEvent a)

{-|
FromJSON instance for input-output events.
-}
instance (FromJSON a) => FromJSON (TIOEvent a )

{-|
ToJSON instance for input-output events.
-}
instance (ToJSON a) => ToJSON (TIOEvent a)

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
Communication input-output events with internal events (CTIOEvents).
-}
data CTIOEvent a
  = CTTau -- ^ internal action (non-observable)
  | CTReceive a -- ^ reception of a call
  | CTReply a -- ^ reply to a call
  | CTInvoke a -- ^ passing a call (= invocation)
  | CTResult a -- ^ getting the result of a call
  deriving (Eq, Ord, Generic)

{-|
Communication input-output events (CIOEvents).
-}
data CIOEvent a
  = CReceive a -- ^ reception of a call
  | CReply a -- ^ reply to a call
  | CInvoke a -- ^ passing a call (= invocation)
  | CResult a -- ^ getting the result of a call
  deriving (Eq, Ord, Generic)

liftToCTIOEvent :: CIOEvent a -> CTIOEvent a
liftToCTIOEvent (CReceive a) = CTReceive a
liftToCTIOEvent (CReply a) = CTReply a
liftToCTIOEvent (CInvoke a) = CTInvoke a
liftToCTIOEvent (CResult a) = CTResult a
 
{-|
Hash instance for communication input-output events.
-}
instance Hashable a => Hashable (CTIOEvent a)

{-|
FromJSON instance for communication input-output events.
-}
instance (FromJSON a) => FromJSON (CTIOEvent a )

{-|
ToJSON instance for communication input-output events.
-}
instance (ToJSON a) => ToJSON (CTIOEvent a)

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
Input-output LTS (TIOLTS).

This is an LTS where labels are input-output events with internal events.
-}
type TIOLTS a = LTS (TIOEvent a)

{-|
Input-output LTS (IOLTS).

This is an LTS where labels are input-output events.
-}
type IOLTS a = LTS (IOEvent a)

{-|
Communication input-output LTS (CTIOLTS).

This is an LTS where labels are communication input-output events with internal events.
-}
type CTIOLTS a = LTS (CTIOEvent a)

{-|
Communication input-output LTS (CIOLTS).

This is an LTS where labels are communication input-output events.
-}
type CIOLTS a = LTS (CIOEvent a)

{-|
Show instance for input-output events with internal events.
-}
instance Show a => Show (TIOEvent a) where
  show TTau         = "tau"
  show (TReceive a) = "receive " ++ show a
  show (TSend a)    = "send " ++ show a

{-|
Show instance for input-output events.
-}
instance Show a => Show (IOEvent a) where
  show (Receive a) = "receive " ++ show a
  show (Send a)    = "send " ++ show a

{-|
Complementary instance for input-output events with internal events.
-}
instance Complementary (TIOEvent a) where
  complementary TTau         = TTau
  complementary (TReceive a) = TSend a
  complementary (TSend a)    = TReceive a

{-|
Complementary instance for input-output events.
-}
instance Complementary (IOEvent a) where
  complementary (Receive a) = Send a
  complementary (Send a)    = Receive a

{-|
Internal instance for input-output events with internal events.
-}
instance Internal (TIOEvent a) where
  isInternal TTau = True
  isInternal _   = False

{-|
Communication instance for input-output events with internal events.
-}
instance TCommunication (TIOEvent a) where
  isInput (TReceive _) = True
  isInput _           = False

{-|
Communication instance for input-output events.
-}
instance Communication (IOEvent a) where
  isInput (Receive _) = True
  isInput _           = False

{-|
Show instance for communication input-output events with internal events.
-}
instance Show a => Show (CTIOEvent a) where
  show CTTau         = "tau"
  show (CTReceive a) = "receive " ++ show a
  show (CTReply a)   = "reply " ++ show a
  show (CTInvoke a)  = "invoke " ++ show a
  show (CTResult a)  = "result " ++ show a

{-|
Show instance for communication input-output events.
-}
instance Show a => Show (CIOEvent a) where
  show (CReceive a) = "receive " ++ show a
  show (CReply a)   = "reply " ++ show a
  show (CInvoke a)  = "invoke " ++ show a
  show (CResult a)  = "result " ++ show a

{-|
Complementary instance for communication input-output events with internal events.
-}
instance Complementary (CTIOEvent a) where
  complementary CTTau         = CTTau
  complementary (CTReceive a) = CTInvoke a
  complementary (CTReply a)   = CTResult a
  complementary (CTInvoke a)  = CTReceive a
  complementary (CTResult a)  = CTReply a

{-|
Complementary instance for communication input-output events.
-}
instance Complementary (CIOEvent a) where
  complementary (CReceive a) = CInvoke a
  complementary (CReply a)   = CResult a
  complementary (CInvoke a)  = CReceive a
  complementary (CResult a)  = CReply a

{-|
Internal instance for communication input-output events with internal events.
-}
instance Internal (CTIOEvent a) where
  isInternal CTTau = True
  isInternal _    = False

{-|
TCommunication instance for communication input-output events with internal events.
-}
instance TCommunication (CTIOEvent a) where
  isInput (CTReceive _) = True
  isInput (CTResult _)  = True
  isInput _            = False

{-|
Communication instance for communication input-output events.
-}
instance Communication (CIOEvent a) where
  isInput (CReceive _) = True
  isInput (CResult _)  = True
  isInput _            = False
