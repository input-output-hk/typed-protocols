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
module Network.TypedProtocol.Stateful.Peer.Client
  ( Client
  , pattern Effect
  , pattern Yield
  , pattern Await
  , pattern Done
  ) where

import           Data.Kind (Type)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Stateful.Peer (Peer)
import qualified Network.TypedProtocol.Stateful.Peer as TP


type Client :: forall ps
            -> ps
            -> (ps -> Type)
            -> (Type -> Type)
            -> Type
            -> Type
type Client ps st f m a = Peer ps AsClient st f m a


-- | Client role pattern for 'TP.Effect'.
--
pattern Effect :: forall ps st f m a.
                  m (Client ps st f m a)
               -- ^ monadic continuation
               -> Client ps st f m a
pattern Effect mclient = TP.Effect mclient


-- | Client role pattern for 'TP.Yield'
--
pattern Yield :: forall ps st f m a.
                 ()
              => forall st'.
                 ( StateTokenI st
                 , StateTokenI st'
                 , StateAgency st ~ ClientAgency
                 )
              => f st'
              -> Message ps st st'
              -- ^ protocol message
              -> Client ps st' f m a
              -- ^ continuation
              -> Client ps st  f m a
pattern Yield f msg k = TP.Yield ReflClientAgency f msg k


-- | Client role pattern for 'TP.Await'
--
pattern Await :: forall ps st f m a.
                 ()
              => ( StateTokenI st
                 , StateAgency st ~ ServerAgency
                 )
              => (forall st'.
                     f st
                  -> Message ps st st'
                  -> ( Client ps st' f m a
                     , f st'
                     )
                 )
              -- ^ continuation
              -> Client ps st f m a
pattern Await k = TP.Await ReflServerAgency k


-- | Client role pattern for 'TP.Done'
--
pattern Done :: forall ps st f m a.
                ()
             => ( StateTokenI st
                , StateAgency st ~ NobodyAgency
                )
             => a
             -- ^ protocol return value
             -> Client ps st f m a
pattern Done a = TP.Done ReflNobodyAgency a


{-# COMPLETE Effect, Yield, Await, Done #-}
