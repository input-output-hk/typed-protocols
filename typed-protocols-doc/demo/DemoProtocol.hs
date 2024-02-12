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

module DemoProtocol
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

data DemoProtocol a where
  -- | Idle state: server waits for ping.
  IdleState :: DemoProtocol a

  -- | Awaiting pong state: server has received ping, client waits for pong.
  AwaitingPongState :: DemoProtocol a

  -- | End state: either side has terminated the session
  EndState :: DemoProtocol a

instance Protocol (DemoProtocol a) where
  data Message (DemoProtocol a) st st' where
    PingMessage :: Message (DemoProtocol a) IdleState AwaitingPongState
    PongMessage :: Message (DemoProtocol a) AwaitingPongState IdleState
    ComplexPongMessage :: Message (DemoProtocol a) AwaitingPongState IdleState
    EndMessage :: Message (DemoProtocol a) st EndState

  data ServerHasAgency st where
    TokIdle :: ServerHasAgency IdleState

  data ClientHasAgency st where
    TokAwaitingPongState :: ClientHasAgency AwaitingPongState

  data NobodyHasAgency st where
    TokEnd :: NobodyHasAgency EndState


  exclusionLemma_ClientAndServerHaveAgency tok1 tok2 =
    case tok1 of
      TokAwaitingPongState -> case tok2 of {}

  exclusionLemma_NobodyAndClientHaveAgency tok1 tok2 =
    case tok1 of
      TokEnd -> case tok2 of {}

  exclusionLemma_NobodyAndServerHaveAgency tok1 tok2 =
    case tok1 of
      TokEnd -> case tok2 of {}

data DemoCodec a

instance Codec (DemoCodec a) where
  type MonadEncode (DemoCodec a) = Identity
  type MonadDecode (DemoCodec a) = Except String

data PongEnum = NormalPong | ComplexPong
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

data PingEnum = PingRequest | EndPing
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

deriving via (ViaEnum PongEnum)
  instance (Codec codec, HasInfo codec (DefEnumEncoding codec)) => HasInfo codec PongEnum

deriving via (ViaEnum PingEnum)
  instance (Codec codec, HasInfo codec (DefEnumEncoding codec)) => HasInfo codec PingEnum

instance HasInfo (DemoCodec b) () where
  info _ _ = basicField "()" (FixedSize 0)

instance HasInfo (DemoCodec b) Text where
  info codec _ =
    compoundField "Text"
      [ ("length", info codec (Proxy @Word32))
      , ("data", basicField "UTF8 dat" (FixedSize 0))
      ]

instance HasInfo (DemoCodec b) a => HasInfo (DemoCodec b) [a] where
  info codec (_ :: Proxy [a]) =
    compoundField "List"
      [ ( "length", info codec (Proxy @Word32))
      , ( "values"
        , listField (VarSize "length") (info codec (Proxy @a))
        )
      ]


instance HasInfo (DemoCodec b) a => HasInfo (DemoCodec b) (Maybe a) where
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

instance HasInfo (DemoCodec b) (Message (DemoProtocol a) IdleState AwaitingPongState) where
  info codec _ = infoOf "PingRequest" $ info codec (Proxy @PingEnum)

instance HasInfo (DemoCodec b) (Message (DemoProtocol a) st EndState) where
  info codec _ = infoOf "EndPing" $ info codec (Proxy @PingEnum)

instance HasInfo (DemoCodec a) Word16 where
  info _ _ = basicField "Word16" (FixedSize 2)

instance HasInfo (DemoCodec a) Word32 where
  info _ _ = basicField "Word32" (FixedSize 4)

instance HasInfo (DemoCodec a) Word64 where
  info _ _ = basicField "Word64" (FixedSize 8)

$(deriveSerDoc ''DemoCodec [] ''PongInfo)

instance HasInfo (DemoCodec b) (Message (DemoProtocol a) AwaitingPongState IdleState) where
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
