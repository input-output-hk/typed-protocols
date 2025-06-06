{-# LANGUAGE TypeFamilies         #-}
-- @UndecidableInstances@ extensions is required for defining @Show@ instance
-- of @'TraceSendRecv'@.
{-# LANGUAGE UndecidableInstances #-}

-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Network.TypedProtocol.Driver.Simple
  ( -- * Introduction
    -- $intro
    -- * Normal peers
    runPeer
  , TraceSendRecv (..)
  , Role (..)
    -- * Pipelined peers
  , runPipelinedPeer
    -- * Connected peers
  , runConnectedPeers
  , runConnectedPeersPipelined
  , runConnectedPeersAsymmetric
    -- * Driver utilities
    -- | This may be useful if you want to write your own driver.
  , driverSimple
  , runDecoderWithChannel
  ) where

import Network.TypedProtocol.Channel
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Network.TypedProtocol.Peer

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer (..), contramap, traceWith)


-- $intro
--
-- A 'Peer' is a particular implementation of an agent that engages in a
-- typed protocol. To actualy run one we need a source and sink for the typed
-- protocol messages. These are provided by a 'Channel' and a 'Codec'. The
-- 'Channel' represents one end of an untyped duplex message transport, and
-- the 'Codec' handles conversion between the typed protocol messages and
-- the untyped channel.
--
-- So given the 'Peer' and a compatible 'Codec' and 'Channel' we can run the
-- peer in some appropriate monad. The peer and codec have to agree on
-- the same protocol and role in that protocol. The codec and channel have to
-- agree on the same untyped medium, e.g. text or bytes. All three have to
-- agree on the same monad in which they will run.
--
-- This module provides drivers for normal and pipelined peers. There is
-- very little policy involved here so typically it should be possible to
-- use these drivers, and customise things by adjusting the peer, or codec
-- or channel.
--
-- It is of course possible to write custom drivers and the code for these ones
-- may provide a useful starting point. The 'runDecoder' function may be a
-- helpful utility for use in custom drives.
--

-- | Structured 'Tracer' output for 'runPeer' and derivitives.
--
data TraceSendRecv ps where
     TraceSendMsg :: AnyMessage ps -> TraceSendRecv ps
     TraceRecvMsg :: AnyMessage ps -> TraceSendRecv ps

instance Show (AnyMessage ps) => Show (TraceSendRecv ps) where
  show (TraceSendMsg msg) = "Send " ++ show msg
  show (TraceRecvMsg msg) = "Recv " ++ show msg


driverSimple :: forall ps pr failure bytes m.
                (MonadThrow m, Exception failure)
             => Tracer m (TraceSendRecv ps)
             -> Codec ps failure m bytes
             -> Channel m bytes
             -> Driver ps pr (Maybe bytes) m
driverSimple tracer Codec{encode, decode} channel@Channel{send} =
    Driver { sendMessage, recvMessage, initialDState = Nothing }
  where
    sendMessage :: forall (st :: ps) (st' :: ps).
                   ( StateTokenI st
                   , ActiveState st
                   )
                => ReflRelativeAgency (StateAgency st)
                                       WeHaveAgency
                                      (Relative pr (StateAgency st))
                -> Message ps st st'
                -> m ()
    sendMessage !_refl msg = do
      send (encode msg)
      traceWith tracer (TraceSendMsg (AnyMessage msg))

    recvMessage :: forall (st :: ps).
                   ( StateTokenI st
                   , ActiveState st
                   )
                => ReflRelativeAgency (StateAgency st)
                                       TheyHaveAgency
                                      (Relative pr (StateAgency st))
                -> Maybe bytes
                -> m (SomeMessage st, Maybe bytes)
    recvMessage !_refl trailing = do
      decoder <- decode stateToken
      result  <- runDecoderWithChannel channel trailing decoder
      case result of
        Right x@(SomeMessage msg, _trailing') -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          return x
        Left failure ->
          throwIO failure


-- | Run a peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeer
  :: forall ps (st :: ps) pr failure bytes m a.
     (MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> Peer ps pr 'NonPipelined st m a
  -> m (a, Maybe bytes)
runPeer tracer codec channel peer =
    runPeerWithDriver driver peer
  where
    driver = driverSimple tracer codec channel


-- | Run a pipelined peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadSTM' constraint.
--
runPipelinedPeer
  :: forall ps (st :: ps) pr failure bytes m a.
     (MonadAsync m, MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe bytes)
runPipelinedPeer tracer codec channel peer =
    runPipelinedPeerWithDriver driver peer
  where
    driver = driverSimple tracer codec channel


--
-- Utils
--

-- | Run a codec incremental decoder 'DecodeStep' against a channel. It also
-- takes any extra input data and returns any unused trailing data.
--
runDecoderWithChannel :: Monad m
                      => Channel m bytes
                      -> Maybe bytes
                      -> DecodeStep bytes failure m a
                      -> m (Either failure (a, Maybe bytes))

runDecoderWithChannel Channel{recv} = go
  where
    go _ (DecodeDone x trailing)         = return (Right (x, trailing))
    go _ (DecodeFail failure)            = return (Left failure)
    go Nothing         (DecodePartial k) = recv >>= k        >>= go Nothing
    go (Just trailing) (DecodePartial k) = k (Just trailing) >>= go Nothing


data Role = Client | Server
  deriving Show

-- | Run two 'Peer's via a pair of connected 'Channel's and a common 'Codec'.
--
-- This is useful for tests and quick experiments.
--
-- The first argument is expected to create two channels that are connected,
-- for example 'createConnectedChannels'.
--
runConnectedPeers :: (MonadAsync m, MonadCatch m,
                      Exception failure)
                  => m (Channel m bytes, Channel m bytes)
                  -> Tracer m (Role, TraceSendRecv ps)
                  -> Codec ps failure m bytes
                  -> Peer ps             pr  'NonPipelined st m a
                  -> Peer ps (FlipAgency pr) 'NonPipelined st m b
                  -> m (a, b)
runConnectedPeers createChannels tracer codec client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPeer tracerClient codec clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer

runConnectedPeersPipelined :: (MonadAsync m, MonadCatch m,
                               Exception failure)
                           => m (Channel m bytes, Channel m bytes)
                           -> Tracer m (PeerRole, TraceSendRecv ps)
                           -> Codec ps failure m bytes
                           -> PeerPipelined ps             pr                st m a
                           -> Peer          ps (FlipAgency pr) 'NonPipelined st m b
                           -> m (a, b)
runConnectedPeersPipelined createChannels tracer codec client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPipelinedPeer tracerClient codec clientChannel client)
      `concurrently`
    (fst <$> runPeer          tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) AsClient) tracer
    tracerServer = contramap ((,) AsServer) tracer


-- Run the same protocol with different codes.  This is useful for testing
-- 'Handshake' protocol which knows how to decode different versions.
--
runConnectedPeersAsymmetric
    :: ( MonadAsync      m
       , MonadMask       m
       , Exception failure
       )
    => m (Channel m bytes, Channel m bytes)
    -> Tracer m (Role, TraceSendRecv ps)
    -> Codec ps failure m bytes
    -> Codec ps failure m bytes
    -> PeerPipelined ps             pr                st m a
    -> Peer          ps (FlipAgency pr) 'NonPipelined st m b
    -> m (a, b)
runConnectedPeersAsymmetric createChannels tracer codec codec' client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPipelinedPeer tracerClient codec  clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec' serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer
