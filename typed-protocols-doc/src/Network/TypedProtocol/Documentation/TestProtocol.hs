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

module Network.TypedProtocol.Documentation.TestProtocol
where

import Network.TypedProtocol.Core
import Data.SerDoc.Info
import Control.Monad.Identity
import Control.Monad.Except
import Data.Proxy
import Data.Word
import Data.Typeable
import Data.SerDoc.Class
import Data.Text (Text)

data TestProtocol a where
  -- | Idle state: server waits for ping.
  IdleState :: TestProtocol a

  -- | Awaiting pong state: server has received ping, client waits for pong.
  AwaitingPongState :: TestProtocol a

  -- | End state: either side has terminated the session
  EndState :: TestProtocol a

instance Protocol (TestProtocol a) where
  data Message (TestProtocol a) st st' where
    PingMessage :: Message (TestProtocol a) IdleState AwaitingPongState
    PongMessage :: Message (TestProtocol a) AwaitingPongState IdleState
    MadPongMessage :: Message (TestProtocol a) AwaitingPongState IdleState
    EndMessage :: Message (TestProtocol a) st EndState

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

data TestCodec a

instance Codec (TestCodec a) where
  type MonadEncode (TestCodec a) = Identity
  type MonadDecode (TestCodec a) = Except String

data PongEnum = NormalPong | MadPong
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

data PingEnum = PingRequest | EndPing
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

deriving via (ViaEnum PongEnum)
  instance (Codec codec, HasInfo codec (DefEnumEncoding codec)) => HasInfo codec PongEnum

deriving via (ViaEnum PingEnum)
  instance (Codec codec, HasInfo codec (DefEnumEncoding codec)) => HasInfo codec PingEnum

instance HasInfo (TestCodec b) () where
  info codec _ = basicField "()" (FixedSize 0)

instance HasInfo (TestCodec b) Text where
  info codec _ =
    compoundField "Text"
      [ ("length", info codec (Proxy @Word32))
      , ("data", basicField "UTF8 dat" (FixedSize 0))
      ]

instance HasInfo (TestCodec b) a => HasInfo (TestCodec b) [a] where
  info codec (p :: Proxy [a]) =
    compoundField "List"
      [ ( "length", info codec (Proxy @Word32))
      , ( "values"
        , listField (VarSize "length") (info codec (Proxy @a))
        )
      ]


instance HasInfo (TestCodec b) a => HasInfo (TestCodec b) (Maybe a) where
  info codec (p :: Proxy (Maybe a)) =
    compoundField "Maybe"
      [ ("isJust", info codec (Proxy @Word32))
      , ( "value"
        , sumField "isJust"
            [ ("Nothing", info codec (Proxy @()))
            , ("Just", info codec (Proxy @a))
            ]
        )
      ]

instance HasInfo (TestCodec b) (Message (TestProtocol a) IdleState AwaitingPongState) where
  info codec _ = infoOf "PingRequest" $ info codec (Proxy @PingEnum)

instance HasInfo (TestCodec b) (Message (TestProtocol a) st EndState) where
  info codec _ = infoOf "EndPing" $ info codec (Proxy @PingEnum)

instance HasInfo (TestCodec a) Word16 where
  info _ _ = basicField "Word16" (FixedSize 2)

instance HasInfo (TestCodec a) Word32 where
  info _ _ = basicField "Word32" (FixedSize 4)

instance HasInfo (TestCodec a) Word64 where
  info _ _ = basicField "Word64" (FixedSize 8)

instance HasInfo (TestCodec b) (Message (TestProtocol a) AwaitingPongState IdleState) where
  info codec _ = infoOf "NormalPong" $ info codec (Proxy @PongEnum)

instance HasInfo (TestCodec b) (Message (TestProtocol a) AwaitingPongState AwaitingPongState) where
  info codec _ = infoOf "MadPong" $ info codec (Proxy @PongEnum)
