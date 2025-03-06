module Network.TypedProtocol.PingPong.Codec.CBOR where

import Control.Monad.Class.MonadST

import Data.ByteString.Lazy (ByteString)

import Codec.CBOR.Decoding qualified as CBOR (Decoder, decodeWord)
import Codec.CBOR.Encoding qualified as CBOR (Encoding, encodeWord)
import Codec.CBOR.Read qualified as CBOR

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.CBOR
import Network.TypedProtocol.Core
import Network.TypedProtocol.PingPong.Type

codecPingPong
  :: forall m.
     MonadST m
  => Codec PingPong CBOR.DeserialiseFailure m ByteString
codecPingPong = mkCodecCborLazyBS encodeMsg decodeMsg
 where
  encodeMsg :: forall st st'.
               Message PingPong st st'
            -> CBOR.Encoding
  encodeMsg MsgPing = CBOR.encodeWord 0
  encodeMsg MsgPong = CBOR.encodeWord 1
  encodeMsg MsgDone = CBOR.encodeWord 2

  decodeMsg :: forall s (st :: PingPong).
               ActiveState st
            => StateToken st
            -> CBOR.Decoder s (SomeMessage st)
  decodeMsg stok = do
    key <- CBOR.decodeWord
    case (stok, key) of
      (SingIdle, 0)   -> return $ SomeMessage MsgPing
      (SingBusy, 1)   -> return $ SomeMessage MsgPong
      (SingIdle, 2)   -> return $ SomeMessage MsgDone

      -- TODO proper exceptions
      (SingIdle, _)   -> fail "codecPingPong.StIdle: unexpected key"
      (SingBusy, _)   -> fail "codecPingPong.StBusy: unexpected key"
      (a@SingDone, _) -> notActiveState a
