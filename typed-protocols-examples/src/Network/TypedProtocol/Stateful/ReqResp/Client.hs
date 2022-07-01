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
    -- * Pipelined Client
  , ReqRespClientPipelined (..)
  , ReqRespIdle (..)
  , reqRespClientPeerPipelined
  ) where

import           Control.Monad.Class.MonadSTM (STM)

import           Data.Kind (Type)

import           Network.TypedProtocol.Core
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
  -> Client  (ReqResp req resp) NonPipelined Empty StIdle f m stm a

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

--
-- Pipelined client
--

-- | A request-response client designed for running the 'ReqResp' protocol in
-- a pipelined way.
--
data ReqRespClientPipelined req resp (f :: ReqResp req resp -> Type) m a where
  -- | A 'PingPongSender', but starting with zero outstanding pipelined
  -- responses, and for any internal collect type @c@.
  ReqRespClientPipelined ::
      ReqRespIdle            req resp Empty f m a
   -> ReqRespClientPipelined req resp       f m a


data ReqRespIdle req resp (q :: Queue (ReqResp req resp)) (f :: ReqResp req resp -> Type) m a where
  -- | Send a `Req` message but alike in `ReqRespClient` do not await for the
  -- response, instead supply a monadic action which will run on a received
  -- `Pong` message.
  SendMsgReqPipelined
    :: f StBusy
    -> req
    -> ReqRespIdle req resp (q |> Tr StBusy StIdle) f m a  -- continuation
    -> ReqRespIdle req resp  q                      f m a

  CollectPipelined
    :: IsLast (ReqResp req resp) q StIdle
    => Maybe      (ReqRespIdle req resp   (Tr StBusy StIdle <| q) f m a)
    -> (f StBusy -> resp -> ( m (ReqRespIdle req resp          q  f m a)
                            , f StIdle
                            ))
    ->             ReqRespIdle req resp   (Tr StBusy StIdle <| q) f m a

  -- | Termination of the req-resp protocol.
  SendMsgDonePipelined
    :: f StDone -> a -> ReqRespIdle req resp Empty f m a

-- | Interpret a pipelined client as a 'Peer' on the client side of
-- the 'ReqResp' protocol.
--
reqRespClientPeerPipelined
  :: Functor m
  => ReqRespClientPipelined req resp f                   m         a
  -> Client (ReqResp req resp) 'Pipelined Empty StIdle f m (STM m) a
reqRespClientPeerPipelined (ReqRespClientPipelined peer) =
    reqRespClientPeerIdle peer


reqRespClientPeerIdle
  :: forall req resp (q :: Queue (ReqResp req resp)) (f :: ReqResp req resp -> Type) m a.
     Functor m
  => ReqRespIdle   req resp               q        f m         a
  -> Client (ReqResp req resp) 'Pipelined q StIdle f m (STM m) a

reqRespClientPeerIdle = go
  where
    go :: forall (q' :: Queue (ReqResp req resp)).
          ReqRespIdle     req resp             q'        f m         a
       -> Client (ReqResp req resp) 'Pipelined q' StIdle f m (STM m) a

    go (SendMsgReqPipelined f req next) =
      -- Pipelined yield: send `MsgReq`, immediately follow with the next step.
      -- Await for a response in a continuation.
      YieldPipelined
        f
        (MsgReq req)
        (go next)

    go (CollectPipelined mNone collect) =
      Collect
        (go <$> mNone)
        (\f (MsgResp resp) -> case collect f resp of
          (next, f') -> ( CollectDone $ Effect $ go <$> next
                        , f'
                        ))

    go (SendMsgDonePipelined f result) =
      -- Send `MsgDone` and complete the protocol
      Yield f MsgDone (Done result)
