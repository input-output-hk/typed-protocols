{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.Stateful.ReqResp.Server
  ( ReqRespServer (..)
  , reqRespServerPeer
  ) where

import           Data.Typeable
import           Network.TypedProtocol.Stateful.Peer.Server
import           Network.TypedProtocol.Stateful.ReqResp.Type


data ReqRespServer req m a = ReqRespServer {
    reqRespServerDone :: a,
    reqRespHandleReq  :: forall resp. Typeable resp => req resp -> m (resp, ReqRespServer req m a)
  }

reqRespServerPeer :: Functor m
                  => ReqRespServer req m a
                  -> Server (ReqResp req) StIdle State m a
reqRespServerPeer ReqRespServer { reqRespServerDone = a,
                                  reqRespHandleReq  = k } =
  Await $ \_ -> \case
    MsgDone -> (Done a, StateDone)
    MsgReq req ->
      ( Effect $
              (\(resp, k') -> Yield StateIdle (MsgResp req resp) (reqRespServerPeer  k'))
          <$> k req
      , StateBusy req
      )
