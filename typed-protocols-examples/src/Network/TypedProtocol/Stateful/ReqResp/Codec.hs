{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Network.TypedProtocol.Stateful.ReqResp.Codec where

import           Data.Kind (Type)
import           Data.Singletons.Decide
import           Data.Typeable
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.PingPong.Codec (decodeTerminatedFrame)
import           Network.TypedProtocol.Stateful.Codec
import           Network.TypedProtocol.Stateful.ReqResp.Type

data Some (f :: k -> Type) where
    Some :: Typeable a => f a -> Some f


-- | Codec polymorphic in the RPC (e.g. `req` type)
--
codecReqResp
  :: forall req m. Monad m
  => (forall resp. req resp -> String)
  -- ^ encode `req resp`
  -> (String -> Maybe (Some req))
  -- ^ decode `req resp`
  -> (forall resp. resp -> String)
  -- ^ encode resp
  -> (forall resp. req resp -> String -> Maybe resp)
  -- ^ decode resp
  -> Codec (ReqResp req) CodecFailure State m String
codecReqResp encodeReq decodeReq encodeResp decodeResp =
    Codec { encode, decode }
  where
    encode :: State st'
           -> Message (ReqResp req) st st'
           -> String
    encode _ (MsgReq req)   = "MsgReq " ++ encodeReq req ++ "\n"
    encode _ MsgDone        = "MsgDone\n"
    encode _ (MsgResp resp) = "MsgResp " ++ encodeResp resp ++ "\n"

    decode :: forall (st :: ReqResp req).
              ActiveState st
           => StateToken st
           -> State st
           -> m (DecodeStep String CodecFailure m (SomeMessage st))
    decode stok state =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, state, break (==' ') str) of
          (SingIdle, StateIdle, ("MsgReq", str'))
            |  Just (Some req) <- decodeReq str'
            -> DecodeDone (SomeMessage (MsgReq req)) trailing
          (SingIdle, StateIdle, ("MsgDone", ""))
            -> DecodeDone (SomeMessage MsgDone) trailing
          (SingBusy, StateBusy req, ("MsgResp", str'))
            -- note that we need `req` to decode response of the given type
            |  Just resp <- decodeResp req str'
            -> DecodeDone (SomeMessage (MsgResp resp)) trailing
          (_, _, _) -> DecodeFail failure
            where failure = CodecFailure ("unexpected server message: " ++ str)


data Bytes where
    Bytes :: Message (ReqResp FileAPI) st st' -> Bytes

-- | An identity codec which wraps messages into `AnyMessage`.
--
codecReqRespId
  :: forall m.
     Applicative m
  => (forall (res1 :: Type) (res2 :: Type).
           (Typeable res1, Typeable res2)
        => Proxy res1
        -> Proxy res2
        -> Maybe (res1 :~: res2)
     )
  -> Codec FileRPC String State m Bytes
codecReqRespId eqRespTypes = Codec { encode, decode }
  where
    encode _ = Bytes

    decode :: forall (st :: ReqResp FileAPI).
              ActiveState st
           => StateToken st
           -> State st
           -> m (DecodeStep Bytes String m (SomeMessage st))
    decode stok state = pure $ DecodePartial $ \bytes -> pure $
      case (stok, state, bytes) of
        (SingIdle, StateIdle, Just (Bytes msg@MsgDone))
          -> DecodeDone (SomeMessage msg) Nothing
        (SingIdle, StateIdle, Just (Bytes msg@MsgReq{}))
          -> DecodeDone (SomeMessage msg) Nothing
        (SingBusy, StateBusy req, Just (Bytes msg@(MsgResp _)))
          -- the codec needs to verify that response type of `req` and `msg` agrees
          |  Just Refl <- eqRespTypes (reqRespType req) (msgRespType msg)
          -> DecodeDone (SomeMessage msg) Nothing

        (SingDone, _, _) -> notActiveState stok
        (_, _, Nothing) -> DecodeFail "no bytes"
        (_, _, _) -> DecodeFail "no matching message"

    msgRespType :: forall resp. Message (ReqResp FileAPI) (StBusy resp) StIdle
                -> Proxy resp
    msgRespType (MsgResp _) = Proxy

    reqRespType :: forall resp. FileAPI resp -> Proxy resp
    reqRespType _ = Proxy


