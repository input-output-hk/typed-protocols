{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

-- | Actions for running 'Peer's with a 'Driver'.  This module should be
-- imported qualified.
--
module Network.TypedProtocol.Stateful.Driver
  ( -- * DriverIngerface
    Driver (..)
    -- * Running a peer
  , runPeerWithDriver
    -- * Re-exports
  , SomeMessage (..)
  , DecodeStep (..)
  ) where

import Control.Monad.Class.MonadSTM

import Data.Kind (Type)

import Network.TypedProtocol.Codec (DecodeStep (..), SomeMessage (..))
import Network.TypedProtocol.Core
import Network.TypedProtocol.Stateful.Peer

data Driver ps (pr :: PeerRole) bytes failure dstate f m =
        Driver {
          -- | Send a message.
          --
          sendMessage   :: forall (st :: ps) (st' :: ps).
                           StateTokenI st
                        => StateTokenI st'
                        => ActiveState st
                        => ReflRelativeAgency (StateAgency st)
                                               WeHaveAgency
                                              (Relative pr (StateAgency st))
                        -> f st'
                        -> Message ps st st'
                        -> m ()

        , -- | Receive a message, a blocking action which reads from the network
          -- and runs the incremental decoder until a full message is decoded.
          --
          recvMessage   :: forall (st :: ps).
                           StateTokenI st
                        => ActiveState st
                        => ReflRelativeAgency (StateAgency st)
                                               TheyHaveAgency
                                              (Relative pr (StateAgency st))
                        -> f st
                        -> dstate
                        -> m (SomeMessage st, dstate)

        , initialDState :: dstate
        }


--
-- Running peers
--

-- | Run a peer with the given driver.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeerWithDriver
  :: forall ps (st :: ps) pr bytes failure dstate (f :: ps -> Type) m a.
     MonadSTM m
  => Driver ps pr bytes failure dstate f m
  -> f st
  -> Peer ps pr st f m a
  -> m (a, dstate)
runPeerWithDriver Driver{ sendMessage
                        , recvMessage
                        , initialDState
                        } =
    go initialDState
  where
    go :: forall st'.
          dstate
       -> f st'
       -> Peer ps pr st' f m a
       -> m (a, dstate)
    go !dstate !f (Effect k) = k >>= go dstate f

    go !dstate  _ (Done _ x) = return (x, dstate)

    go !dstate  _ (Yield refl !f msg k) = do
      sendMessage refl f msg
      go dstate f k

    go !dstate !f (Await refl k) = do
      (SomeMessage msg, dstate') <- recvMessage refl f dstate
      case k f msg of
        (k', f') -> go dstate' f' k'
