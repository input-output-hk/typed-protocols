{-# LANGUAGE CPP #-}

module Network.TypedProtocol.Channel
  ( Channel (..)
  , hoistChannel
  , isoKleisliChannel
  , fixedInputChannel
  , mvarsAsChannel
  , handlesAsChannel
#if !defined(mingw32_HOST_OS)
  , socketAsChannel
#endif
  , createConnectedChannels
  , createConnectedBufferedChannels
  , createConnectedBufferedChannelsUnbounded
  , createPipelineTestChannels
  , channelEffect
  , delayChannel
  , loggingChannel
  ) where

import Control.Concurrent.Class.MonadSTM
import Control.Monad ((>=>))
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTimer.SI
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Internal (smallChunkSize)
import Data.Proxy
import Numeric.Natural

#if !defined(mingw32_HOST_OS)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy qualified as Socket
#endif

import System.IO qualified as IO (Handle, hFlush, hIsEOF)


-- | One end of a duplex channel. It is a reliable, ordered channel of some
-- medium. The medium does not imply message boundaries, it can be just bytes.
--
data Channel m a = Channel {

       -- | Write output to the channel.
       --
       -- It may raise exceptions (as appropriate for the monad and kind of
       -- channel).
       --
       send :: a -> m (),

       -- | Read some input from the channel, or @Nothing@ to indicate EOF.
       --
       -- Note that having received EOF it is still possible to send.
       -- The EOF condition is however monotonic.
       --
       -- It may raise exceptions (as appropriate for the monad and kind of
       -- channel).
       --
       recv :: m (Maybe a)
     }


-- | Given an isomorphism between @a@ and @b@ (in Kleisli category), transform
-- a @'Channel' m a@ into @'Channel' m b@.
--
isoKleisliChannel
  :: forall a b m. Monad m
  => (a -> m b)
  -> (b -> m a)
  -> Channel m a
  -> Channel m b
isoKleisliChannel f finv Channel{send, recv} = Channel {
    send = finv >=> send,
    recv = recv >>= traverse f
  }


hoistChannel
  :: (forall x . m x -> n x)
  -> Channel m a
  -> Channel n a
hoistChannel nat channel = Channel
  { send = nat . send channel
  , recv = nat (recv channel)
  }

-- | A 'Channel' with a fixed input, and where all output is discarded.
--
-- The input is guaranteed to be supplied via 'read' with the given chunk
-- boundaries.
--
-- This is only useful for testing. In particular the fixed chunk boundaries
-- can be used to test that framing and other codecs work with any possible
-- chunking.
--
fixedInputChannel :: MonadSTM m => [a] -> m (Channel m a)
fixedInputChannel xs0 = do
    v <- atomically $ newTVar xs0
    return Channel {send, recv = recv v}
  where
    recv v = atomically $ do
               xs <- readTVar v
               case xs of
                 []      -> return Nothing
                 (x:xs') -> writeTVar v xs' >> return (Just x)

    send _ = return ()


-- | Make a 'Channel' from a pair of 'TMVar's, one for reading and one for
-- writing.
--
mvarsAsChannel :: MonadSTM m
               => TMVar m a
               -> TMVar m a
               -> Channel m a
mvarsAsChannel bufferRead bufferWrite =
    Channel{send, recv}
  where
    send x = atomically (putTMVar bufferWrite x)
    recv   = atomically (Just <$> takeTMVar bufferRead)


-- | Create a pair of channels that are connected via one-place buffers.
--
-- This is primarily useful for testing protocols.
--
createConnectedChannels :: forall m a. (MonadLabelledSTM m, MonadTraceSTM m, Show a) => m (Channel m a, Channel m a)
createConnectedChannels = do
    -- Create two TMVars to act as the channel buffer (one for each direction)
    -- and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ do
      v <- newEmptyTMVar
      labelTMVar v "buffer-a"
      traceTMVar (Proxy @m) v $ \_ a -> pure $ TraceString ("buffer-a: " ++ show a)
      return v
    bufferB <- atomically $ do
      v <- newEmptyTMVar
      traceTMVar (Proxy @m) v $ \_ a -> pure $ TraceString ("buffer-b: " ++ show a)
      labelTMVar v "buffer-b"
      return v

    return (mvarsAsChannel bufferB bufferA,
            mvarsAsChannel bufferA bufferB)


-- | Create a pair of channels that are connected via N-place buffers.
--
-- This variant /blocks/ when 'send' would exceed the maximum buffer size.
-- Use this variant when you want the environment rather than the 'Peer' to
-- limit the pipelining.
--
-- This is primarily useful for testing protocols.
--
createConnectedBufferedChannels :: MonadSTM m
                                => Natural -> m (Channel m a, Channel m a)
createConnectedBufferedChannels sz = do
    -- Create two TBQueues to act as the channel buffers (one for each
    -- direction) and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ newTBQueue sz
    bufferB <- atomically $ newTBQueue sz

    return (queuesAsChannel bufferB bufferA,
            queuesAsChannel bufferA bufferB)
  where
    queuesAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send x = atomically (writeTBQueue bufferWrite x)
        recv   = atomically (Just <$> readTBQueue bufferRead)


-- | Create a pair of channels that are connected via two unbounded buffers.
--
-- This is primarily useful for testing protocols.
--
createConnectedBufferedChannelsUnbounded :: forall m a. MonadSTM m
                                         => m (Channel m a, Channel m a)
createConnectedBufferedChannelsUnbounded = do
    -- Create two TQueues to act as the channel buffers (one for each
    -- direction) and use them to make both ends of a bidirectional channel
    bufferA <- newTQueueIO
    bufferB <- newTQueueIO

    return (queuesAsChannel bufferB bufferA,
            queuesAsChannel bufferA bufferB)
  where
    queuesAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send x  = atomically (writeTQueue bufferWrite x)
        recv    = atomically (     Just <$> readTQueue bufferRead)

-- | Create a pair of channels that are connected via N-place buffers.
--
-- This variant /fails/ when  'send' would exceed the maximum buffer size.
-- Use this variant when you want the 'Peer' to limit the pipelining itself,
-- and you want to check that it does not exceed the expected level of
-- pipelining.
--
-- This is primarily useful for testing protocols.
--
createPipelineTestChannels :: MonadSTM m
                           => Natural -> m (Channel m a, Channel m a)
createPipelineTestChannels sz = do
    -- Create two TBQueues to act as the channel buffers (one for each
    -- direction) and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ newTBQueue sz
    bufferB <- atomically $ newTBQueue sz

    return (queuesAsChannel bufferB bufferA,
            queuesAsChannel bufferA bufferB)
  where
    queuesAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send x = atomically $ do
                   full <- isFullTBQueue bufferWrite
                   if full then error failureMsg
                           else writeTBQueue bufferWrite x
        recv   = atomically (Just <$> readTBQueue bufferRead)

    failureMsg = "createPipelineTestChannels: "
              ++ "maximum pipeline depth exceeded: " ++ show sz


-- | Make a 'Channel' from a pair of IO 'Handle's, one for reading and one
-- for writing.
--
-- The Handles should be open in the appropriate read or write mode, and in
-- binary mode. Writes are flushed after each write, so it is safe to use
-- a buffering mode.  On unix named pipes can be used, see
-- 'Network.TypedProtocol.ReqResp.Test.prop_namedPipePipelined_IO'
--
-- For bidirectional handles it is safe to pass the same handle for both.
--
handlesAsChannel :: IO.Handle -- ^ Read handle
                 -> IO.Handle -- ^ Write handle
                 -> Channel IO LBS.ByteString
handlesAsChannel hndRead hndWrite =
    Channel{send, recv}
  where
    send :: LBS.ByteString -> IO ()
    send chunk = do
      LBS.hPut hndWrite chunk
      IO.hFlush hndWrite

    recv :: IO (Maybe LBS.ByteString)
    recv = do
      eof <- IO.hIsEOF hndRead
      if eof
        then return Nothing
        else Just . LBS.fromStrict <$> BS.hGetSome hndRead smallChunkSize


-- | Transform a channel to add an extra action before /every/ send and after
-- /every/ receive.
--
channelEffect :: forall m a.
                 Monad m
              => (a -> m ())        -- ^ Action before 'send'
              -> (Maybe a -> m ())  -- ^ Action after 'recv'
              -> Channel m a
              -> Channel m a
channelEffect beforeSend afterRecv Channel{send, recv} =
    Channel{
      send = \x -> do
        beforeSend x
        send x

    , recv = do
        mx <- recv
        afterRecv mx
        return mx
    }

-- | Delay a channel on the receiver end.
--
-- This is intended for testing, as a crude approximation of network delays.
-- More accurate models along these lines are of course possible.
--
delayChannel :: MonadDelay m
             => DiffTime
             -> Channel m a
             -> Channel m a
delayChannel delay = channelEffect (\_ -> return ())
                                   (\_ -> threadDelay delay)


#if !defined(mingw32_HOST_OS)
socketAsChannel :: Socket
                -> Channel IO LBS.ByteString
socketAsChannel sock =
    Channel{send, recv}
  where
    send :: LBS.ByteString -> IO ()
    send = Socket.sendAll sock

    recv :: IO (Maybe LBS.ByteString)
    recv = do
      bs <- Socket.recv sock (fromIntegral smallChunkSize)
      if LBS.null bs
        then return Nothing
        else return (Just bs)
#endif

-- | Channel which logs sent and received messages.
--
loggingChannel :: ( MonadSay m
                  , Show id
                  , Show a
                  )
               => id
               -> Channel m a
               -> Channel m a
loggingChannel ident Channel{send,recv} =
  Channel {
    send = loggingSend,
    recv = loggingRecv
  }
 where
  loggingSend a = do
    say (show ident ++ ":send:" ++ show a)
    send a

  loggingRecv = do
    msg <- recv
    case msg of
      Nothing -> return ()
      Just a  -> say (show ident ++ ":recv:" ++ show a)
    return msg
