{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.TypedProtocol.Tests.ControlProtocol where

import Network.TypedProtocol.Documentation
import Network.TypedProtocol.Tests.TestProtocol (TestCodec)

import Data.ByteString ( ByteString )
import Data.Kind
import Data.SerDoc.Class
import Data.SerDoc.Info
import Data.SerDoc.TH
import Data.Text ( Text )
import Data.Typeable
import Data.Word
import Network.TypedProtocol.Core

data AgentInfo =
  AgentInfo
    { agentInfoCurrentBundle :: !(Maybe BundleInfo)
    , agentInfoStagedKey :: !(Maybe KeyInfo)
    , agentInfoBootstrapConnections :: ![BootstrapInfo]
    }
    deriving (Show, Eq)

data Command
  = GenStagedKeyCmd
  | QueryStagedKeyCmd
  | DropStagedKeyCmd
  | InstallKeyCmd
  | RequestInfoCmd
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

deriving via (ViaEnum Command)
  instance
    ( Codec codec
    , HasInfo codec (DefEnumEncoding codec)
    ) => HasInfo codec Command

instance
  ( Codec codec
  , Serializable codec (DefEnumEncoding codec)
  , Integral (DefEnumEncoding codec)
  , Monad (MonadEncode codec)
  , Monad (MonadDecode codec)
  ) => Serializable codec Command where
    encode codec = encodeEnum codec (Proxy @(DefEnumEncoding codec))
    decode codec = decodeEnum codec (Proxy @(DefEnumEncoding codec))

newtype FakeKey = FakeKey { fakeKeyData :: ByteString }
  deriving (Show, Eq, Ord)

newtype VersionIdentifier = VersionIdentifier { versionIdentifierData :: ByteString }
  deriving (Show, Eq, Ord)

instance HasInfo TestCodec VersionIdentifier where
  info _ _ = basicField "Bytes" (FixedSize 32)

instance HasInfo TestCodec FakeKey where
  info _ _ = basicField "Bytes" (FixedSize 128)

data BootstrapInfo =
  BootstrapInfo
    { bootstrapInfoAddress :: !Text
    , bootstrapInfoStatus :: !ConnectionStatus
    }
    deriving (Show, Eq)

data ConnectionStatus
  = ConnectionUp
  | ConnectionConnecting
  | ConnectionDown
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

deriving via (ViaEnum ConnectionStatus)
  instance (Codec codec, HasInfo codec (DefEnumEncoding codec))
    => HasInfo codec ConnectionStatus

data BundleInfo =
  BundleInfo
    { bundleInfoEvolution :: !Word32
    , bundleInfoOCertN :: !Word64
    , bundleInfoVK :: !FakeKey
    }
    deriving (Show, Eq)

newtype KeyInfo =
  KeyInfo
    { keyInfoVK :: FakeKey
    }
    deriving (Show, Eq)

deriving newtype
  instance
    ( HasInfo codec FakeKey
    , Codec codec
    ) => HasInfo codec KeyInfo

$(deriveSerDoc ''TestCodec [] ''BundleInfo)
$(deriveSerDoc ''TestCodec [] ''BootstrapInfo)
$(deriveSerDoc ''TestCodec [] ''AgentInfo)

-- | The protocol for pushing KES keys.
--
-- Intended use:
--
-- - The Agent acts as the Client, and the Control Server as a Server
-- - When the Control Server connects, it pushes a key to the Agent
-- - The Agent stores the key locally in memory and pushes it to any connected
--   Nodes.
--
-- All pushes are confirmed from the receiving end, to make sure they have gone
-- through. This allows the control client to report success to the user, but it
-- also helps make things more predictable in testing, because it means that
-- sending keys is now synchronous.
data ControlProtocol (m :: Type -> Type) (c :: Type) where
  -- | Default state after connecting, but before the protocol version has been
  -- negotiated.
  InitialState :: ControlProtocol m c

  -- | System is idling, waiting for the server to push the next key.
  IdleState :: ControlProtocol m c

  -- | Client has requested a new KES key to be generated in the staging area.
  WaitForPublicKeyState :: ControlProtocol m c

  -- | Client has requested agent information
  WaitForInfoState :: ControlProtocol m c

  -- | An OpCert has been pushed, client must now confirm that it has been
  -- received, and that it matches the staged KES key.
  WaitForConfirmationState :: ControlProtocol m c

  -- | The server has closed the connection, thus signalling the end of the
  -- session.
  EndState :: ControlProtocol m c

{-# ANN VersionMessage (Description ["Announce the protocol version."]) #-}
{-# ANN GenStagedKeyMessage
    (Description
      [ "Ask the agent to generate a fresh KES sign key and store it in the staging area."
      , "Corresponds to the @gen-staged-key@ command."
      ]) #-}
{-# ANN QueryStagedKeyMessage
    (Description
      [ "Ask the agent to return the staged KES key, if any."
      , "Only the KES verification key will be returned, in order to guarantee forward security."
      , "Corresponds to the @export-staged-key@ command."
      ]) #-}
{-# ANN DropStagedKeyMessage
    (Description
      [ "Ask the agent to delete the staged KES key, if any."
      , "Corresponds to the @drop-staged-key@ command."
      ]) #-}
{-# ANN PublicKeyMessage
    (Description
      [ "Returned by the KES agent in response to an @export-staged-key@ request."
      ]) #-}
{-# ANN InstallKeyMessage
    (Description
      [ "Upload an OpCert to the KES agent, and ask it to bundle it with a staged KES key and install it."
      , "Corresponds to the @install-key@ command."
      ]) #-}
{-# ANN InstallResultMessage
    (Description
      [ "Returned by the KES agent in response to an @install-key@ command."
      ]) #-}
{-# ANN RequestInfoMessage
    (Description
      [ "Ask the KES agent to report its current state."
      , "Corresponds to the @info@ command."
      ]) #-}
{-# ANN InfoMessage
    (Description
      [ "Returned by the KES agent in response to an @info@ command."
      ]) #-}
{-# ANN AbortMessage
    (Description
      [ "Signals a failed version handshake."
      , "No data is actually sent for this message, instead it is generated when the network connection is interrupted."
      ]) #-}
{-# ANN EndMessage
    (Description
      [ "Signals an orderly end of a session."
      , "No data is actually sent for this message, instead it is generated when the network connection is interrupted."
      ]) #-}
{-# ANN ProtocolErrorMessage
    (Description
      [ "Signals a fatal protocol error that causes the session to end prematurely."
      , "No data is actually sent for this message, instead it is generated when the network connection is interrupted, or an unrecoverable error occurs."
      ]) #-}

instance Protocol (ControlProtocol m c) where
  data Message (ControlProtocol m c) st st' where

          VersionMessage :: Message (ControlProtocol m c) InitialState IdleState

          GenStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState

          QueryStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState

          DropStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState

          PublicKeyMessage :: Maybe FakeKey
                           -> Message (ControlProtocol m c) WaitForPublicKeyState IdleState

          InstallKeyMessage :: FakeKey
                            -> Message (ControlProtocol m c) IdleState WaitForConfirmationState

          InstallResultMessage :: Word32
                               -> Message (ControlProtocol m c) WaitForConfirmationState IdleState

          RequestInfoMessage :: Message (ControlProtocol m c) IdleState WaitForInfoState

          InfoMessage :: AgentInfo
                      -> Message (ControlProtocol m c) WaitForInfoState IdleState

          AbortMessage :: Message (ControlProtocol m c) InitialState EndState
          EndMessage :: Message (ControlProtocol m c) IdleState EndState
          ProtocolErrorMessage :: Message (ControlProtocol m c) a EndState

  -- | Server always has agency, except between sending a key and confirming it
  type StateAgency InitialState = ServerAgency
  type StateAgency IdleState = ServerAgency
  type StateAgency WaitForConfirmationState = ClientAgency
  type StateAgency WaitForPublicKeyState = ClientAgency
  type StateAgency WaitForInfoState = ClientAgency
  type StateAgency EndState = NobodyAgency

  type StateToken = SControlProtocol

data SControlProtocol (st :: ControlProtocol m c) where
  SInitialState :: SControlProtocol InitialState
  SIdleState :: SControlProtocol IdleState
  SWaitForConfirmationState :: SControlProtocol WaitForConfirmationState
  SWaitForPublicKeyState :: SControlProtocol WaitForPublicKeyState
  SWaitForInfoState :: SControlProtocol WaitForInfoState
  SEndState :: SControlProtocol EndState

instance HasInfo TestCodec (Message (ControlProtocol m c) InitialState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              "InitialState,IdleState" ++
              ">")
            (info codec (Proxy @VersionIdentifier))
instance HasInfo TestCodec (Message (ControlProtocol m c) IdleState WaitForPublicKeyState) where
  info codec _ = aliasField
            ("Message<" ++
              "IdleState,WaitForPublicKeyState" ++
              ">")
            (info codec (Proxy @Command))

--  infoOf c = case c of
--    GenStagedKeyMessage -> infoOf GenStagedKeyCmd
--    QueryStagedKeyMessage -> infoOf QueryStagedKeyCmd
--    DropStagedKeyMessage -> infoOf DropStagedKeyCmd

instance (HasInfo TestCodec FakeKey) => HasInfo TestCodec (Message (ControlProtocol m c) WaitForPublicKeyState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              "WaitForPublicKeyState,IdleState" ++
              ">")
            (info codec (Proxy @(Maybe FakeKey)))
instance (HasInfo TestCodec FakeKey) => HasInfo TestCodec (Message (ControlProtocol m c) IdleState WaitForConfirmationState) where
  info codec _ = aliasField
            ("Message<" ++
              "IdleState,WaitForConfirmationState" ++
              ">")
            (info codec (Proxy @FakeKey))
instance HasInfo TestCodec (Message (ControlProtocol m c) WaitForConfirmationState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              "WaitForConfirmationState,IdleState" ++
              ">")
            (info codec (Proxy @Word32))
instance HasInfo TestCodec (Message (ControlProtocol m c) IdleState WaitForInfoState) where
  info codec _ = aliasField
            ("Message<" ++
              "IdleState,WaitForInfoState" ++
              ">")
            (info codec (Proxy @()))
instance ( HasInfo TestCodec FakeKey
         , HasInfo TestCodec AgentInfo
         ) => HasInfo TestCodec (Message (ControlProtocol m c) WaitForInfoState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              "WaitForInfoState,IdleState" ++
              ">")
            (info codec (Proxy @AgentInfo))
instance HasInfo TestCodec (Message (ControlProtocol m c) _st EndState) where
  info codec _ = aliasField
            ("Message<" ++
              "st,EndState" ++
              ">")
            (info codec (Proxy @()))
