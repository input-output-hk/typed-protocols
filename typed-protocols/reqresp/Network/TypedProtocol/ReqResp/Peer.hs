{-# LANGUAGE TypeFamilies #-}

module Network.TypedProtocol.ReqResp.Peer where

import Data.Type.NatMap

import Network.TypedProtocol.ReqResp.Core as Core

-- simplifying assumptions
--
-- The there is only one state on the client, from which it can send requests
-- landing in different server states.  The server must respond to each
-- request, but it doesn't need to respond in the same order as the requests
-- arrived, it can re-order them as it wishes.

--
-- NOTE:
-- Leios doesn't directly fit into this model, because it has:
-- 
-- > StIdle -> StBlockRange -> StBlockRange -> StIdle
--
-- cycle in its state diagram.  However, we can model this by putting
-- `StBlockRange` n-times into the state, saying that the server needs to handle
-- n requests before the protocol can terminate.
--
-- NOTE:
-- CIP is suggesting that we are going to use similar mini-protocol approach as
-- we use in praos.  However, Leios freshest-first delivery is not compatible
-- with it, and thus it will need to be changed.
--

type Peer :: forall ps
          -- ^ protocol, which indexes all request types
          -> PeerRole ps
          -> (Type -> Type)
          -- ^ monad's kind
          -> Type
          -> Type
data Peer ps pr m a where

  Effect
    :: forall ps pr m a.
       m (Peer ps pr m a)
    -- ^ monadic continuation
    ->    Peer ps pr m a

  --
  -- Client
  --

  -- | Send a request to the server side.
  --
  SendRequest
    :: forall ps (st :: ps) m a.
       RequestTypeI st
    => Request ps (Active st)
    -- ^ protocol message
    -> (Response ps st -> m ())
    -- handle response with some side effects
    -> Peer ps (AsClient Running) m a
    -- ^ continuation
    -> Peer ps (AsClient Running) m a

  SendRequestDone
    :: forall ps m a.
       Request ps Terminal
    -> Peer ps (AsClient Terminated) m a
    -> Peer ps (AsClient Running) m a

  ClientDone
    :: forall ps m a.
       a
    -> Peer ps (AsClient Terminated) m a

  --
  -- Server
  --

  SendResponse
    :: forall ps (q :: NatMap (State ps)) (st :: ps) (n :: N) m a.
       ( Lookup (Active st) q ~ S n
       , RequestTypeI st
       )
    => Response ps st
    -> Peer ps (AsServer (Decrement (Active st) q)) m a
    -> Peer ps (AsServer q) m a

  AwaitRequest
    :: forall ps q m a.
       Lookup Terminal q ~ Z
       -- server cannot await for more request if `Terminal` request was
       -- already received, client cannot send anything either.
    => (forall (st :: State ps).
             Request ps st
          -> Peer ps (AsServer (Increment st q)) m a
       )
       -- ^ continuation
    ->  Peer ps (AsServer q) m a

  ServerDone
    :: forall ps m a.
       a
    -> Peer ps (AsServer (Cons (Terminal :-> S Z) Empty)) m a
