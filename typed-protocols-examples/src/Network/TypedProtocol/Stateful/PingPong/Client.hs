{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Network.TypedProtocol.Stateful.PingPong.Client
  ( -- * Non-pipelined peer
    PingPongClient (..)
  , pingPongClientPeer
    -- * Pipelined peer
  , PingPongClientPipelined (..)
  , PingPongClientIdle
  , pingPongClientPeerPipelined
  , pingPongClientPeerPipelinedSTM
  ) where

import           Control.Monad.Class.MonadSTM

import           Data.Kind (Type)
import           Data.Type.Queue

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.PingPong.Type
import           Network.TypedProtocol.Stateful.Peer.Client


data PingPongClient (f :: PingPong -> Type) m a where
  -- | Choose to go for sending a ping message. The ping has no body so
  -- all we have to provide here is a continuation for the single legal
  -- reply message.
  --
  SendMsgPing    :: f StBusy
                 -> m (PingPongClient f m a) -- continuation for Pong response
                 -> PingPongClient f m a

  -- | Choose to terminate the protocol. This is an actual but nullary message,
  -- we terminate with the local result value. So this ends up being much like
  -- 'return' in this case, but in general the termination is a message that
  -- can communicate final information.
  --
  SendMsgDone    :: f StDone -> a -> PingPongClient f m a


pingPongClientPeer
  :: Functor m
  => (f StBusy -> f StIdle)
  -> PingPongClient f m a
  -> Client PingPong NonPipelined Empty StIdle f m stm (a, f StDone)

pingPongClientPeer _busytoIdle (SendMsgDone f result) =
    Yield f MsgDone (Done (result, f))

pingPongClientPeer busyToIdle (SendMsgPing f next) =
    Yield f MsgPing $
    Await $ \f' MsgPong ->
      ( Effect $ pingPongClientPeer busyToIdle <$> next
      , busyToIdle f'
      )


data PingPongClientPipelined (f :: PingPong -> Type) m a where
  -- | A 'PingPongSender', but starting with zero outstanding pipelined
  -- responses, and for any internal collect type @c@.
  PingPongClientPipelined
    :: PingPongClientIdle      Empty f m a
    -> PingPongClientPipelined       f m a


data PingPongClientIdle (q :: Queue PingPong) (f :: PingPong -> Type) m a where
  -- | Send a `Ping` message but alike in `PingPongClient` do not await for the
  -- response, instead supply a monadic action which will run on a received
  -- `Pong` message.
  --
  SendMsgPingPipelined
    :: f StBusy
    -> PingPongClientIdle (q |> Tr StBusy StIdle) f m a -- continuation
    -> PingPongClientIdle  q                      f m a

  -- | Collect the result of a previous pipelined receive action.
  --
  -- This (optionally) provides two choices:
  --
  -- * Continue without a pipelined result
  -- * Continue with a pipelined result, which allows to run a monadic action
  --   when 'MsgPong' is received.
  --
  -- Since presenting the first choice is optional, this allows expressing
  -- both a blocking collect and a non-blocking collect. This allows
  -- implementations to express policies such as sending a short sequence
  -- of messages and then waiting for all replies, but also a maximum pipelining
  -- policy that keeps a large number of messages in flight but collects results
  -- eagerly.
  --
  CollectPipelined
    :: IsLast PingPong q StIdle
    => Maybe (PingPongClientIdle (Tr StBusy StIdle <| q) f m a)
    -> m     (PingPongClientIdle                      q  f m a)
    ->        PingPongClientIdle (Tr StBusy StIdle <| q) f m a

  -- | Termination of the ping-pong protocol.
  --
  -- Note that all pipelined results must be collected before terminating.
  --
  SendMsgDonePipelined
    :: f StDone -> a -> PingPongClientIdle Empty f m a


pingPongClientPeerPipelined
  :: Functor m
  => (f StBusy -> f StIdle)
  -> PingPongClientPipelined f m a
  -> Client PingPong Pipelined Empty StIdle f m (STM m) a
pingPongClientPeerPipelined busyToIdle (PingPongClientPipelined peer) =
    pingPongClientPeerIdle busyToIdle peer

pingPongClientPeerIdle
  :: forall (q :: Queue PingPong) (f :: PingPong -> Type) m a.
     Functor m
  => (f StBusy -> f StIdle)
  -> PingPongClientIdle                q f m         a
  -> Client PingPong Pipelined q StIdle f m (STM m) a
pingPongClientPeerIdle busyToIdle = go
  where
    go :: forall (q' :: Queue PingPong).
          PingPongClientIdle         q'        f m         a
       -> Client PingPong Pipelined q' StIdle f m (STM m) a

    go (SendMsgPingPipelined f next) =
      YieldPipelined f MsgPing (go next)

    go (CollectPipelined mNone collect) =
      Collect
        (go <$> mNone)
        (\f MsgPong -> ( CollectDone $ Effect (go <$> collect)
                       , busyToIdle f
                       ))

    go (SendMsgDonePipelined f result) =
      Yield f MsgDone (Done result)


pingPongClientPeerPipelinedSTM
  :: MonadSTM m
  => (f StBusy -> f StIdle)
  -> PingPongClientPipelined f m a
  -> Client PingPong Pipelined Empty StIdle f m (STM m) a
pingPongClientPeerPipelinedSTM busyToIdle (PingPongClientPipelined peer) =
    pingPongClientPeerIdleSTM busyToIdle peer

pingPongClientPeerIdleSTM
  :: forall (q :: Queue PingPong) (f :: PingPong -> Type) m a.
     MonadSTM m
  => (f StBusy -> f StIdle)
  -> PingPongClientIdle                q f m         a
  -> Client PingPong Pipelined q StIdle f m (STM m) a
pingPongClientPeerIdleSTM busyToIdle = go
  where
    go :: forall (q' :: Queue PingPong).
          PingPongClientIdle         q'        f m         a
       -> Client PingPong Pipelined q' StIdle f m (STM m) a

    go (SendMsgPingPipelined f next) =
      YieldPipelined f MsgPing (go next)

    go (CollectPipelined mNone collect) =
      CollectSTM
        (maybe retry (pure . go) mNone)
        (\f MsgPong -> ( CollectDone $ Effect (go <$> collect)
                       , busyToIdle f
                       ))

    go (SendMsgDonePipelined f result) =
      Yield f MsgDone (Done result)
