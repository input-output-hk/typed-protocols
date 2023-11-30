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

data TestProtocol a where
  -- | Idle state: server waits for ping.
  IdleState :: TestProtocol a

  -- | Awaiting pong state: server has received ping, client waits for pong.
  AwaitingPongState :: TestProtocol a

instance Protocol (TestProtocol a) where
  data Message (TestProtocol a) st st' where
    PingMessage :: Message (TestProtocol a) IdleState AwaitingPongState
    PongMessage :: Message (TestProtocol a) AwaitingPongState IdleState
    MadPongMessage :: Message (TestProtocol a) AwaitingPongState IdleState

  data ServerHasAgency st where
    TokIdle :: ServerHasAgency IdleState

  data ClientHasAgency st where
    TokAwaitingPongState :: ClientHasAgency AwaitingPongState

  data NobodyHasAgency st where


  exclusionLemma_ClientAndServerHaveAgency tok1 tok2 =
    case tok1 of
      TokAwaitingPongState -> case tok2 of {}

  exclusionLemma_NobodyAndClientHaveAgency tok1 _ =
    case tok1 of {}

  exclusionLemma_NobodyAndServerHaveAgency tok1 _ =
    case tok1 of {}

data TestCodec

instance Codec TestCodec where
  type MonadEncode TestCodec = Identity
  type MonadDecode TestCodec = Except String
  type Encoded TestCodec = ()
  type Context TestCodec = ()

data PongEnum = NormalPong | MadPong
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

deriving via (ViaEnum PongEnum)
  instance (Codec codec, HasInfo codec (DefEnumEncoding codec)) => HasInfo codec PongEnum

instance HasInfo TestCodec (Message (TestProtocol a) IdleState AwaitingPongState) where
  info _ _ = basicField "PingMessage" (FixedSize 0)

instance HasInfo TestCodec Word16 where
  info _ _ = basicField "Word16" (FixedSize 2)

instance HasInfo TestCodec Word32 where
  info _ _ = basicField "Word32" (FixedSize 4)

instance HasInfo TestCodec (Message (TestProtocol a) AwaitingPongState IdleState) where
  info codec _ =
    sumField "PongMessage"
      [ ("PongMessage", infoOf "NormalPong" $ info codec (Proxy @PongEnum))
      , ("MadPongMessage", infoOf "MadPong" $ info codec (Proxy @PongEnum))
      ]
