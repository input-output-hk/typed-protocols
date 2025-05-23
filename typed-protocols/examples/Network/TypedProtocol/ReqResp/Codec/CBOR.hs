module Network.TypedProtocol.ReqResp.Codec.CBOR where

import Control.Monad.Class.MonadST

import Data.ByteString.Lazy (ByteString)

import Codec.CBOR.Decoding qualified as CBOR (Decoder, decodeListLen,
           decodeWord)
import Codec.CBOR.Encoding qualified as CBOR (Encoding, encodeListLen,
           encodeWord)
import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise.Class (Serialise)
import Codec.Serialise.Class qualified as CBOR

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.CBOR
import Network.TypedProtocol.Core
import Network.TypedProtocol.ReqResp.Type

codecReqResp
  :: forall req resp m.
     ( MonadST m
     , Serialise req
     , Serialise resp
     )
  => Codec (ReqResp req resp) CBOR.DeserialiseFailure m ByteString
codecReqResp = mkCodecCborLazyBS encodeMsg decodeMsg
 where
  encodeMsg :: forall st st'.
               Message (ReqResp req resp) st st'
            -> CBOR.Encoding
  encodeMsg (MsgReq req) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> CBOR.encode req
  encodeMsg (MsgResp resp) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> CBOR.encode resp
  encodeMsg MsgDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 2

  decodeMsg :: forall s (st :: ReqResp req resp).
               ActiveState st
            => StateToken st
            -> CBOR.Decoder s (SomeMessage st)
  decodeMsg stok = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (stok, key) of
      (SingIdle, 0)   -> SomeMessage . MsgReq  <$> CBOR.decode
      (SingBusy, 1)   -> SomeMessage . MsgResp <$> CBOR.decode
      (SingIdle, 2)   -> return $ SomeMessage MsgDone

      -- TODO proper exceptions
      (SingIdle, _)   -> fail "codecReqResp.StIdle: unexpected key"
      (SingBusy, _)   -> fail "codecReqResp.StBusy: unexpected key"
      (a@SingDone, _) -> notActiveState a

