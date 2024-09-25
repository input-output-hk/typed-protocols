{-# LANGUAGE PatternSynonyms #-}

-- | Bidirectional patterns for @'Peer' ps 'AsServer'@.   The advantage of
-- these patterns is that they automatically provide the 'RelativeAgencyEq'
-- singleton.
--
module Network.TypedProtocol.Stateful.Peer.Server
  ( -- * Server type alias and its pattern synonyms
    Server
  , pattern Effect
  , pattern Yield
  , pattern Await
  , pattern Done
  ) where

import Data.Kind (Type)

import Network.TypedProtocol.Core
import Network.TypedProtocol.Stateful.Peer (Peer)
import Network.TypedProtocol.Stateful.Peer qualified as TP


type Server :: forall ps
            -> ps
            -> (ps -> Type)
            -> (Type -> Type)
            -> Type
            -> Type
type Server ps st f m a = Peer ps AsServer st f m a


-- | Server role pattern for 'TP.Effect'.
--
pattern Effect :: forall ps st f m a.
                  m (Server ps st f m a)
               -- ^ monadic continuation
               -> Server ps st f m a
pattern Effect mclient = TP.Effect mclient


-- | Server role pattern for 'TP.Yield'
--
pattern Yield :: forall ps st f m a.
                 ()
              => forall st'.
                 ( StateTokenI st
                 , StateTokenI st'
                 , StateAgency st ~ ServerAgency
                 )
              => f st
              -> f st'
              -> Message ps st st'
              -- ^ protocol message
              -> Server ps st' f m a
              -- ^ continuation
              -> Server ps st  f m a
pattern Yield f f' msg k = TP.Yield ReflServerAgency f f' msg k


-- | Server role pattern for 'TP.Await'
--
pattern Await :: forall ps st f m a.
                 ()
              => ( StateTokenI st
                 , StateAgency st ~ ClientAgency
                 )
              => (forall st'.
                     f st
                  -> Message ps st st'
                  -> ( Server ps st' f m a
                     , f st'
                     )
                 )
              -- ^ continuation
              -> Server ps st  f m a
pattern Await k = TP.Await ReflClientAgency k


-- | Server role pattern for 'TP.Done'
--
pattern Done :: forall ps st f m a.
                ()
             => ( StateTokenI st
                , StateAgency st ~ NobodyAgency
                )
             => a
             -- ^ protocol return value
             -> Server ps st f m a
pattern Done a = TP.Done ReflNobodyAgency a

{-# COMPLETE Effect, Yield, Await, Done #-}
