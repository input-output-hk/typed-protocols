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
  , pattern CollectSTM
  , pattern CollectDone
    -- * re-exports
  , Pipelined (..)
  , Queue (..)
  ) where

import           Control.Monad.Class.MonadSTM (STM)

import           Data.Kind (Type)
import           Data.Singletons

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer (Peer)
import qualified Network.TypedProtocol.Peer as TP


type Server :: forall ps
            -> Pipelined
            -> Queue ps
            -> ps
            -> (Type -> Type)
            -> Type
            -> Type
type Server ps pl q st m a = Peer ps AsServer pl q st m a


-- | Server role pattern for 'TP.Effect'.
--
pattern Effect :: forall ps pl q st m a.
                  m (Server ps pl q st m a)
               -- ^ monadic continuation
               -> Server ps pl q st m a
pattern Effect mclient = TP.Effect mclient


-- | Server role pattern for 'TP.Yield'
--
pattern Yield :: forall ps pl st m a.
                 ()
              => forall st'.
                 ( SingI st
                 , SingI st'
                 , StateAgency st ~ ServerAgency
                 )
              => Message ps st st'
              -- ^ protocol message
              -> Server ps pl Empty st' m a
              -- ^ continuation
              -> Server ps pl Empty st  m a
pattern Yield msg k = TP.Yield ReflServerAgency msg k


-- | Server role pattern for 'TP.Await'
--
pattern Await :: forall ps pl st m a.
                 ()
              => ( SingI st
                 , StateAgency st ~ ClientAgency
                 )
              => (forall st'. Message ps st st'
                  -> Server ps pl Empty st' m a)
              -- ^ continuation
              -> Server     ps pl Empty st  m a
pattern Await k = TP.Await ReflClientAgency k


-- | Server role pattern for 'TP.Done'
--
pattern Done :: forall ps pl st m a.
                ()
             => ( SingI st
                , StateAgency st ~ NobodyAgency
                )
             => a
             -- ^ protocol return value
             -> Server ps pl Empty st m a
pattern Done a = TP.Done ReflNobodyAgency a


-- | Server role pattern for 'TP.YieldPipelined'
--
pattern YieldPipelined :: forall ps st q m a.
                          ()
                       => forall st' st''.
                          ( SingI st
                          , SingI st'
                          , StateAgency st ~ ServerAgency
                          )
                       => Message ps st st'
                       -- ^ pipelined message
                       -> Server ps 'Pipelined (q |> Tr st' st'') st'' m a
                       -- ^ continuation
                       -> Server ps 'Pipelined  q                 st   m a
pattern YieldPipelined msg k = TP.YieldPipelined ReflServerAgency msg k


-- | Server role pattern for 'TP.Collect'
--
pattern Collect :: forall ps st' st'' q st m a.
                   ()
                => ( SingI st'
                   , StateAgency st' ~ ClientAgency
                   )
                => Maybe (Server ps 'Pipelined (Tr st' st'' <| q) st m a)
                -- ^ continuation, executed if no message has arrived so far
                -> (forall stNext. Message ps st' stNext
                    -> Server ps 'Pipelined (Tr stNext st'' <| q) st m a)
                -- ^ continuation
                -> Server     ps 'Pipelined (Tr st'    st'' <| q) st m a
pattern Collect k' k = TP.Collect ReflClientAgency k' k


-- | Client role pattern for 'TP.Collect'
--
pattern CollectSTM :: forall ps st' st'' q st m a.
                      ()
                   => ( SingI st'
                      , StateAgency st' ~ ClientAgency
                      )
                   => STM m (Server ps 'Pipelined (Tr st' st'' <| q) st m a)
                   -- ^ continuation, executed if no message has arrived so far
                   -> (forall stNext. Message ps st' stNext
                      -> Server ps 'Pipelined (Tr stNext st'' <| q) st m a)
                   -- ^ continuation
                   -> Server     ps 'Pipelined (Tr st'    st'' <| q) st m a
pattern CollectSTM k' k = TP.CollectSTM ReflClientAgency k' k


-- | Client role pattern for 'TP.CollectDone'
--
pattern CollectDone :: forall ps st q st' m a.
                       ()
                    => ()
                    => Server ps 'Pipelined              q  st' m a
                    -- ^ continuation
                    -> Server ps 'Pipelined (Tr st st <| q) st' m a
pattern CollectDone k = TP.CollectDone k


{-# COMPLETE Effect, Yield, Await, Done, YieldPipelined, Collect, CollectDone #-}
