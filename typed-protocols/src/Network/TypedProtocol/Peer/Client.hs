{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

-- | Bidirectional patterns for @'Peer' ps 'AsClient'@.   The advantage of
-- these patterns is that they automatically provide the 'RelativeAgencyEq'
-- singleton.
--
module Network.TypedProtocol.Peer.Client
  ( Client
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
import           Data.Singletons

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer (Peer, N (..),
                    Nat (..), Outstanding)
import qualified Network.TypedProtocol.Peer as TP


type Client :: forall ps
            -> IsPipelined
            -> Outstanding
            -> ps
            -> (Type -> Type)
            -> Type
            -> Type
type Client ps pl q st m a = Peer ps AsClient pl q st m a


-- | Client role pattern for 'TP.Effect'.
--
pattern Effect :: forall ps pl n st m a.
                  m (Client ps pl n st m a)
               -- ^ monadic continuation
               -> Client ps pl n st m a
pattern Effect mclient = TP.Effect mclient


-- | Client role pattern for 'TP.Yield'
--
pattern Yield :: forall ps pl st m a.
                 ()
              => forall st'.
                 ( SingI st
                 , SingI st'
                 , StateAgency st ~ ClientAgency
                 )
              => Message ps st st'
              -- ^ protocol message
              -> Client ps pl Z st' m a
              -- ^ continuation
              -> Client ps pl Z st  m a
pattern Yield msg k = TP.Yield ReflClientAgency msg k


-- | Client role pattern for 'TP.Await'
--
pattern Await :: forall ps pl st m a.
                 ()
              => ( SingI st
                 , StateAgency st ~ ServerAgency
                 )
              => (forall st'. Message ps st st'
                  -> Client ps pl Z st' m a)
              -- ^ continuation
              -> Client     ps pl Z st  m a
pattern Await k = TP.Await ReflServerAgency k


-- | Client role pattern for 'TP.Done'
--
pattern Done :: forall ps pl st m a.
                ()
             => ( SingI st
                , StateAgency st ~ NobodyAgency
                )
             => a
             -- ^ protocol return value
             -> Client ps pl Z st m a
pattern Done a = TP.Done ReflNobodyAgency a


-- | Client role pattern for 'TP.YieldPipelined'
--
pattern YieldPipelined :: forall ps st n c m a.
                          ()
                       => forall st' st''.
                          ( SingI st
                          , SingI st'
                          , StateAgency st ~ ClientAgency
                          )
                       => Message ps st st'
                       -- ^ pipelined message
                       -> Receiver ps st' st'' m c
                       -> Client ps (Pipelined c) (S n) st'' m a
                       -- ^ continuation
                       -> Client ps (Pipelined c)    n  st   m a
pattern YieldPipelined msg receiver k = TP.YieldPipelined ReflClientAgency msg receiver k


-- | Client role pattern for 'TP.Collect'
--
pattern Collect :: forall ps st n c m a.
                   ()
                => ( SingI st
                   , ActiveState st
                   )
                => Maybe (Client ps (Pipelined c) (S n) st m a)
                -- ^ continuation, executed if no message has arrived so far
                -> (c ->  Client ps (Pipelined c)    n  st m a)
                -- ^ continuation
                -> Client        ps (Pipelined c) (S n) st m a
pattern Collect k' k = TP.Collect k' k

{-# COMPLETE Effect, Yield, Await, Done, YieldPipelined, Collect #-}


type Receiver ps st stdone m c = TP.Receiver ps AsClient st stdone m c

pattern ReceiverEffect :: forall ps st stdone m c.
                          m (Receiver ps st stdone m c)
                       -> Receiver ps st stdone m c
pattern ReceiverEffect k = TP.ReceiverEffect k

pattern ReceiverAwait :: forall ps st stdone m c.
                         ()
                      => ( SingI st
                         , ActiveState st
                         , StateAgency st ~ ServerAgency
                         )
                      => (forall st'. Message  ps st st'
                                   -> Receiver ps    st' stdone m c
                         )
                      -> Receiver ps st stdone m c
pattern ReceiverAwait k = TP.ReceiverAwait ReflServerAgency k

pattern ReceiverDone :: forall ps stdone m c.
                        c
                     -> Receiver ps stdone stdone m c
pattern ReceiverDone c = TP.ReceiverDone c

{-# COMPLETE ReceiverEffect, ReceiverAwait, ReceiverDone #-}
