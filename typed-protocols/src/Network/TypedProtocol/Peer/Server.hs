{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

-- | Bidirectional patterns for @'Peer' ps 'AsServer'@.   The advantage of
-- these patterns is that they automatically provide the 'RelativeAgencyEq'
-- singleton.
--
module Network.TypedProtocol.Peer.Server
  ( Server
  , pattern Effect
  , pattern Yield
  , pattern Await
  , pattern Done
  , pattern YieldPipelined
  , pattern Collect
  , Receiver
  , pattern ReceiverEffect
  , pattern ReceiverAwait
  , pattern ReceiverDone
    -- * re-exports
  , IsPipelined (..)
  , Outstanding
  , N (..)
  , Nat (..)
  ) where

import           Data.Kind (Type)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer (Peer, N (..),
                    Nat (..), Outstanding)
import qualified Network.TypedProtocol.Peer as TP


type Server :: forall ps
            -> IsPipelined
            -> Outstanding
            -> ps
            -> (Type -> Type)
            -> Type
            -> Type
type Server ps pl q st m a = Peer ps AsServer pl q st m a


-- | Server role pattern for 'TP.Effect'.
--
pattern Effect :: forall ps pl n st m a.
                  m (Server ps pl n st m a)
               -- ^ monadic continuation
               -> Server ps pl n st m a
pattern Effect mclient = TP.Effect mclient


-- | Server role pattern for 'TP.Yield'
--
pattern Yield :: forall ps pl st m a.
                 ()
              => forall st'.
                 ( StateTokenI st
                 , StateTokenI st'
                 , StateAgency st ~ ServerAgency
                 )
              => Message ps st st'
              -- ^ protocol message
              -> Server ps pl Z st' m a
              -- ^ continuation
              -> Server ps pl Z st  m a
pattern Yield msg k = TP.Yield ReflServerAgency msg k


-- | Server role pattern for 'TP.Await'
--
pattern Await :: forall ps pl st m a.
                 ()
              => ( StateTokenI st
                 , StateAgency st ~ ClientAgency
                 )
              => (forall st'. Message ps st st'
                  -> Server ps pl Z st' m a)
              -- ^ continuation
              -> Server     ps pl Z st  m a
pattern Await k = TP.Await ReflClientAgency k


-- | Server role pattern for 'TP.Done'
--
pattern Done :: forall ps pl st m a.
                ()
             => ( StateTokenI st
                , StateAgency st ~ NobodyAgency
                )
             => a
             -- ^ protocol return value
             -> Server ps pl Z st m a
pattern Done a = TP.Done ReflNobodyAgency a


-- | Server role pattern for 'TP.YieldPipelined'
--
pattern YieldPipelined :: forall ps st n c m a.
                          ()
                       => forall st' st''.
                          ( StateTokenI st
                          , StateTokenI st'
                          , StateAgency st ~ ServerAgency
                          )
                       => Message ps st st'
                       -- ^ pipelined message
                       -> Receiver ps st' st'' m c
                       -> Server ps (Pipelined c) (S n) st'' m a
                       -- ^ continuation
                       -> Server ps (Pipelined c)    n  st   m a
pattern YieldPipelined msg receiver k = TP.YieldPipelined ReflServerAgency msg receiver k


-- | Server role pattern for 'TP.Collect'
--
pattern Collect :: forall ps st n c m a.
                   ()
                => ( StateTokenI st
                   , ActiveState st
                   )
                => Maybe (Server ps (Pipelined c) (S n) st m a)
                -- ^ continuation, executed if no message has arrived so far
                -> (c -> Server  ps (Pipelined c)    n  st m a)
                -- ^ continuation
                -> Server        ps (Pipelined c) (S n) st m a
pattern Collect k' k = TP.Collect k' k


{-# COMPLETE Effect, Yield, Await, Done, YieldPipelined, Collect  #-}


type Receiver ps st stdone m c = TP.Receiver ps AsServer st stdone m c

pattern ReceiverEffect :: forall ps st stdone m c.
                          m (Receiver ps st stdone m c)
                       -> Receiver ps st stdone m c
pattern ReceiverEffect k = TP.ReceiverEffect k

pattern ReceiverAwait :: forall ps st stdone m c.
                         ()
                      => ( StateTokenI st
                         , ActiveState st
                         , StateAgency st ~ ClientAgency
                         )
                      => (forall st'. Message  ps st st'
                                   -> Receiver ps    st' stdone m c
                         )
                      -> Receiver ps st stdone m c
pattern ReceiverAwait k = TP.ReceiverAwait ReflClientAgency k

pattern ReceiverDone :: forall ps stdone m c.
                        c
                     -> Receiver ps stdone stdone m c
pattern ReceiverDone c = TP.ReceiverDone c

{-# COMPLETE ReceiverEffect, ReceiverAwait, ReceiverDone #-}
