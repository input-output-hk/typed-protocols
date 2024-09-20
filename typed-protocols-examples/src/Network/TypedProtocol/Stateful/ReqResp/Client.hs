{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.Stateful.ReqResp.Client
  ( ReqRespClient (..)
  , reqRespClientPeer
  ) where

import           Data.Typeable
import           Network.TypedProtocol.Stateful.Peer.Client
import           Network.TypedProtocol.Stateful.ReqResp.Type

data ReqRespClient req m a where
  SendMsgReq  :: Typeable resp
              => req resp
              -> (resp -> m (ReqRespClient req m a))
              -> ReqRespClient req m a

  SendMsgDone :: a
              -> ReqRespClient req m a


reqRespClientPeer
  :: Monad m
  => ReqRespClient req m a
  -> Client (ReqResp req) StIdle State m a

reqRespClientPeer (SendMsgDone a) =
      Yield StateDone MsgDone (Done a)

reqRespClientPeer (SendMsgReq req next) =
    Yield (StateBusy req)
          (MsgReq req) $
    Await $ \_ (MsgResp _ resp) ->
      let client = next resp
      in ( Effect $ reqRespClientPeer <$> client
         , StateIdle
         )
