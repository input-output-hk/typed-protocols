{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Network.TypedProtocol.Stateful.ReqResp.Client
  ( -- * Non-Pipelined Client
    ReqRespClient (..)
  , reqRespClientPeer
  ) where

import           Data.Kind (Type)

import           Network.TypedProtocol.ReqResp.Type
import           Network.TypedProtocol.Stateful.Peer.Client


data ReqRespClient req resp (f :: ReqResp req resp -> Type) m a where
  SendMsgReq  :: f StBusy
              -> req
              -> (f StBusy -> resp -> ( m (ReqRespClient req resp f m a)
                                      , f StIdle
                                      ))
              -> ReqRespClient req resp f m a

  SendMsgDone :: f StDone
              -> m a
              -> ReqRespClient req resp f m a


reqRespClientPeer
  :: Monad m
  => ReqRespClient req resp f m a
  -> Client  (ReqResp req resp) StIdle f m a

reqRespClientPeer (SendMsgDone f result) =
    Effect $ do
      r <- result
      return $ Yield f MsgDone (Done r)

reqRespClientPeer (SendMsgReq f req next) =
    Yield f (MsgReq req) $
    Await $ \f' (MsgResp resp) ->
      case next f' resp of
        (client, f'') ->
          ( Effect $ reqRespClientPeer <$> client
          , f''
          )
