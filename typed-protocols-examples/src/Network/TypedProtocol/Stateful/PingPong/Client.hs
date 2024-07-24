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
  ) where

import           Data.Kind (Type)

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
  -> Client PingPong StIdle f m (a, f StDone)

pingPongClientPeer _busytoIdle (SendMsgDone f result) =
    Yield f MsgDone (Done (result, f))

pingPongClientPeer busyToIdle (SendMsgPing f next) =
    Yield f MsgPing $
    Await $ \f' MsgPong ->
      ( Effect $ pingPongClientPeer busyToIdle <$> next
      , busyToIdle f'
      )
