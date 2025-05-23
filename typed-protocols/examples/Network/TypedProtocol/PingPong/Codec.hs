module Network.TypedProtocol.PingPong.Codec where

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.PingPong.Type


codecPingPong
  :: forall m. Monad m
  => Codec PingPong CodecFailure m String
codecPingPong =
    Codec{encode, decode}
  where
    encode :: forall (st :: PingPong) (st' :: PingPong).
              Message PingPong st st'
           -> String
    encode MsgPing = "ping\n"
    encode MsgDone = "done\n"
    encode MsgPong = "pong\n"

    decode :: forall (st :: PingPong).
              ActiveState st
           => StateToken st
           -> m (DecodeStep String CodecFailure m (SomeMessage st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, str) of
          (SingBusy, "pong") ->
            DecodeDone (SomeMessage MsgPong) trailing
          (SingIdle, "ping") ->
            DecodeDone (SomeMessage MsgPing) trailing
          (SingIdle, "done") ->
            DecodeDone (SomeMessage MsgDone) trailing

          (_       , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected server message: " ++ str)


decodeTerminatedFrame :: forall m a.
                         Monad m
                      => Char
                      -> (String -> Maybe String -> DecodeStep String CodecFailure m a)
                      -> m (DecodeStep String CodecFailure m a)
decodeTerminatedFrame terminator k = go []
  where
    go :: [String] -> m (DecodeStep String CodecFailure m a)
    go chunks =
      return $ DecodePartial $ \mchunk ->
        case mchunk of
          Nothing    -> return $ DecodeFail CodecFailureOutOfInput
          Just chunk ->
            case break (==terminator) chunk of
              (c, _:c') -> return $ k (concat (reverse (c:chunks)))
                                      (if null c' then Nothing else Just c)
              _         -> go (chunk : chunks)



codecPingPongId
  :: forall m. Monad m
  => Codec PingPong CodecFailure m (AnyMessage PingPong)
codecPingPongId =
    Codec{encode,decode}
  where
    encode :: forall (st :: PingPong) (st' :: PingPong)
           .  ( StateTokenI st
              , ActiveState st
              )
           => Message PingPong st st'
           -> AnyMessage PingPong
    encode msg = AnyMessage msg

    decode :: forall (st :: PingPong).
              ActiveState st
           => StateToken st
           -> m (DecodeStep (AnyMessage PingPong) CodecFailure m (SomeMessage st))
    decode stok =
      pure $ DecodePartial $ \mb ->
        case mb of
          Nothing -> return $ DecodeFail (CodecFailure "expected more data")
          Just (AnyMessage msg) -> return $
            case (stok, msg) of
              (SingBusy, MsgPong) ->
                DecodeDone (SomeMessage msg) Nothing
              (SingIdle, MsgPing) ->
                DecodeDone (SomeMessage msg) Nothing
              (SingIdle, MsgDone) ->
                DecodeDone (SomeMessage msg) Nothing

              (SingIdle, _) ->
                DecodeFail failure
                  where failure = CodecFailure ("unexpected client message: " ++ show msg)
              (SingBusy, _) ->
                DecodeFail failure
                  where failure = CodecFailure ("unexpected server message: " ++ show msg)

              (a@SingDone, _) -> notActiveState a
