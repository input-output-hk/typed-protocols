{-# LANGUAGE DeriveLift #-}

module Network.TypedProtocol.Documentation.Types
( AgencyID (..)
, StateRef (..)
, ProtocolDescription (..)
, MessageDescription (..)
, Description (..)
)
where

import Data.SerDoc.Info
import Language.Haskell.TH.Syntax

-- | Represents agency at the term level. Used to indicate which side has
-- agency in a particular protocol state.
data AgencyID
  = ClientAgencyID
  | ServerAgencyID
  | NobodyAgencyID
  deriving (Show, Read, Ord, Eq, Enum, Bounded, Lift)

data StateRef
  = AnyState
  | State !String
  deriving (Show, Read, Ord, Eq)

-- | Term-level representation of a typed protocol.
data ProtocolDescription codec =
  ProtocolDescription
    { protocolName :: String
      -- ^ Human-readable protocol name
    , protocolDescription :: [Description]
    , protocolIdentifier :: String
      -- ^ Machine-readable identifier, may be used for things like protocol
      -- version negotiation.
    , protocolStates :: [(StateRef, [Description], AgencyID)]
      -- ^ List of the protocol's possible states, each entry being a state ID,
      -- a human-readable description, and an indication of agency (client or
      -- server).
    , protocolMessages :: [MessageDescription codec]
    }
    deriving (Show)

-- | Term-level representation of a typed protocol message.
data MessageDescription codec =
  MessageDescription
    { messageName :: String
    , messageDescription :: [Description]
    , messagePayload :: [String]
      -- ^ List of payload values for this message (free-form descriptions or
      -- type names)
    , messageFromState :: StateRef
      -- ^ References a 'protocolState' in the parent 'ProtocolDescription' by
      -- name.
    , messageToState :: StateRef
      -- ^ References a 'protocolState' in the parent 'ProtocolDescription' by
      -- name.
    , messageInfo :: FieldInfo codec
    }
    deriving (Show)

