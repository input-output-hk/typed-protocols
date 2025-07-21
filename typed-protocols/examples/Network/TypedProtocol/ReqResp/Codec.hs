{-# LANGUAGE BlockArguments #-}

module Network.TypedProtocol.ReqResp.Codec where

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.PingPong.Codec (decodeTerminatedFrame)
import Network.TypedProtocol.ReqResp.Type
import Text.Read (readMaybe)


codecReqResp ::
    forall req resp m
  . (Monad m, Show req, Show resp, Read req, Read resp)
  => Codec (ReqResp req resp) CodecFailure m String
codecReqResp =
    Codec{encode, decode}
  where
    encode :: forall req' resp'
                     (st  :: ReqResp req' resp')
                     (st' :: ReqResp req' resp')
           .  ( Show (Message (ReqResp req' resp') st st') )
           => Message (ReqResp req' resp') st st'
           -> String
    encode msg = show msg ++ "\n"

    decode :: forall req' resp' m'
                     (st :: ReqResp req' resp')
           .  (Monad m', Read req', Read resp', ActiveState st)
           => StateToken st
           -> m' (DecodeStep String CodecFailure m' (SomeMessage st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, break (==' ') str) of
          (SingIdle, ("MsgReq", str'))
             | Just req <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgReq req)) trailing
          (SingIdle, ("MsgDone", ""))
            -> DecodeDone (SomeMessage MsgDone) trailing
          (SingBusy, ("MsgResp", str'))
            | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgResp resp)) trailing

          (_       , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected server message: " ++ str)


data WithBytes a = WithBytes {
    bytes   :: String,
    message :: a
  }
  deriving (Show, Eq)

mkWithBytes :: Show a => a -> WithBytes a
mkWithBytes message = WithBytes { bytes = show message, message }


anncodecReqResp ::
    forall req resp m
  .  (Monad m, Show req, Show resp, Read req, Read resp)
  => AnnotatedCodec (ReqResp (WithBytes req) (WithBytes resp)) CodecFailure m String
anncodecReqResp =
      Codec{encode, decode}
  where
    encode :: forall req' resp'
                     (st  :: ReqResp (WithBytes req') (WithBytes resp'))
                     (st' :: ReqResp (WithBytes req') (WithBytes resp'))
           .  ( Show req'
              , Show resp'
              )
           => Message (ReqResp (WithBytes req') (WithBytes resp')) st st'
           -> String
    -- NOTE: we're not using 'Show (Message ...)' instance.  If `req ~ Int`,
    -- then negative numbers will be surrounded with braces (e.g. @"(-1)"@) and
    -- the `Read` type class doesn't have a way to see that brackets were consumed
    -- from the input string.
    encode (MsgReq WithBytes { message })
      = "MsgReq " ++ show message ++ "\n"
    encode (MsgResp WithBytes { message })
      = "MsgResp " ++ show message ++ "\n"
    encode MsgDone
      = "MsgDone" ++ "\n"

    decode :: forall req' resp' m'
                     (st :: ReqResp (WithBytes req') (WithBytes resp'))
           .  (Monad m', Read req', Read resp', ActiveState st)
           => StateToken st
           -> m' (DecodeStep String CodecFailure m' (Annotator String st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, break (==' ') str) of
          (SingIdle, ("MsgReq", str'))
             | Just req <- readMaybe @req' str'
            -> DecodeDone (Annotator \str'' ->
                           let used = init $ drop 7 str'' in
                           SomeMessage (MsgReq (WithBytes used req))) trailing
          (SingIdle, ("MsgDone", ""))
            -> DecodeDone (Annotator \_str'' -> SomeMessage MsgDone) trailing
          (SingBusy, ("MsgResp", str'))
            | Just resp <- readMaybe @resp' str'
            -> DecodeDone (Annotator \str'' ->
                           let used = init $ drop 8 str'' in
                           SomeMessage (MsgResp (WithBytes used resp))) trailing

          (_       , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected server message: " ++ str)



codecReqRespId ::
    forall req resp m
  . (Monad m, Show req, Show resp)
  => Codec (ReqResp req resp) CodecFailure m (AnyMessage (ReqResp req resp))
codecReqRespId =
    Codec{encode, decode}
  where
    encode :: forall (st  :: ReqResp req resp)
                     (st' :: ReqResp req resp)
           .  StateTokenI st
           => ActiveState st
           => Message (ReqResp req resp) st st'
           -> AnyMessage (ReqResp req resp)
    encode msg = AnyMessage msg

    decode :: forall (st :: ReqResp req resp)
           .  ActiveState st
           => StateToken st
           -> m (DecodeStep (AnyMessage (ReqResp req resp)) CodecFailure m (SomeMessage st))
    decode stok =
      pure $ DecodePartial $ \mb ->
        case mb of
          Nothing -> return $ DecodeFail (CodecFailure "expected more data")
          Just (AnyMessage msg) -> return $
            case (stok, msg) of
              (SingIdle, MsgReq{})
                -> DecodeDone (SomeMessage msg) Nothing
              (SingIdle, MsgDone)
                -> DecodeDone (SomeMessage msg) Nothing
              (SingBusy, MsgResp{})
                -> DecodeDone (SomeMessage msg) Nothing

              (SingIdle, _) ->
                DecodeFail failure
                  where failure = CodecFailure ("unexpected client message: " ++ show msg)
              (SingBusy, _) ->
                DecodeFail failure
                  where failure = CodecFailure ("unexpected server message: " ++ show msg)

              (a@SingDone, _) -> notActiveState a
