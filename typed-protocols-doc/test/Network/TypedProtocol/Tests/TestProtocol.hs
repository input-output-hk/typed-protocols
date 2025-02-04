{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.TypedProtocol.Tests.TestProtocol
where

import Network.TypedProtocol.Core
import Data.SerDoc.Info
import Control.Monad.Identity
import Control.Monad.Except
import Data.Proxy
import Data.Word
import Data.Typeable
import Data.SerDoc.Class
import Data.SerDoc.TH
import Data.Text (Text)

data PongInfo =
  PongInfo
    { pongTimestamp :: Word64
    , pongPeerID :: Word64
    , pongMessage :: Text
    }
    deriving (Show, Eq)

data TestProtocol where
  -- | Idle state: server waits for ping.
  IdleState :: TestProtocol

  -- | Awaiting pong state: server has received ping, client waits for pong.
  AwaitingPongState :: TestProtocol

  -- | End state: either side has terminated the session
  EndState :: TestProtocol

instance Protocol TestProtocol where
  data Message TestProtocol st st' where
    PingMessage :: Message TestProtocol IdleState AwaitingPongState
    PongMessage :: Message TestProtocol AwaitingPongState IdleState
    ComplexPongMessage :: Message TestProtocol AwaitingPongState IdleState
    EndMessage :: Message TestProtocol st EndState

  type StateAgency IdleState = ServerAgency
  type StateAgency AwaitingPongState = ClientAgency
  type StateAgency EndState = NobodyAgency

  type StateToken = STestProtocol

data STestProtocol (st :: TestProtocol) where
  SingIdle :: STestProtocol IdleState
  SingAwaitingPong :: STestProtocol AwaitingPongState
  SingEnd :: STestProtocol EndState

instance StateTokenI IdleState where stateToken = SingIdle
instance StateTokenI AwaitingPongState where stateToken = SingAwaitingPong
instance StateTokenI EndState where stateToken = SingEnd

data TestCodec

instance Codec TestCodec where
  type MonadEncode TestCodec = Identity
  type MonadDecode TestCodec = Except String

instance Serializable TestCodec a where
  encode _ = pure (pure ())
  decode _ = throwError "this is a mock codec"

data PongEnum = NormalPong | ComplexPong
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data PingEnum = PingRequest | EndPing
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

deriving via (ViaEnum PongEnum)
  instance (Codec codec, HasInfo codec (DefEnumEncoding codec)) => HasInfo codec PongEnum

deriving via (ViaEnum PingEnum)
  instance (Codec codec, HasInfo codec (DefEnumEncoding codec)) => HasInfo codec PingEnum

instance HasInfo TestCodec () where
  info _ _ = basicField "()" (FixedSize 0)

instance HasInfo TestCodec Text where
  info codec _ =
    compoundField "Text"
      [ ("length", info codec (Proxy @Word32))
      , ("data", basicField "UTF8 dat" (FixedSize 0))
      ]

instance HasInfo TestCodec a => HasInfo TestCodec [a] where
  info codec (_ :: Proxy [a]) =
    compoundField "List"
      [ ( "length", info codec (Proxy @Word32))
      , ( "values"
        , listField (VarSize "length") (info codec (Proxy @a))
        )
      ]


instance HasInfo TestCodec a => HasInfo TestCodec (Maybe a) where
  info codec (_ :: Proxy (Maybe a)) =
    compoundField "Maybe"
      [ ("isJust", info codec (Proxy @Word32))
      , ( "value"
        , sumField "isJust"
            [ ("Nothing", info codec (Proxy @()))
            , ("Just", info codec (Proxy @a))
            ]
        )
      ]

instance HasInfo TestCodec (Message TestProtocol IdleState AwaitingPongState) where
  info codec _ = infoOf "PingRequest" $ info codec (Proxy @PingEnum)

instance HasInfo TestCodec (Message TestProtocol st EndState) where
  info codec _ = infoOf "EndPing" $ info codec (Proxy @PingEnum)

instance HasInfo TestCodec Word16 where
  info _ _ = basicField "Word16" (FixedSize 2)

instance HasInfo TestCodec Word32 where
  info _ _ = basicField "Word32" (FixedSize 4)

instance HasInfo TestCodec Word64 where
  info _ _ = basicField "Word64" (FixedSize 8)

$(deriveSerDoc ''TestCodec [] ''PongInfo)

instance HasInfo TestCodec (Message TestProtocol AwaitingPongState IdleState) where
  info codec _ =
    compoundField "Pong"
      [ ("pongType", info codec (Proxy @PongEnum))
      , ("pongData"
        , choiceField
            (IndexField "pongType")
            [ info codec (Proxy @())
            , info codec (Proxy @PongInfo)
            ]
        )
      ]
