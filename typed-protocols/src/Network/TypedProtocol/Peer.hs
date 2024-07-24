{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

-- | Protocol EDSL.
--
-- __Note__: 'Network.TypedProtocol.Peer.Client.Client' and
-- 'Network.TypedProtocol.Peer.Server.Server' patterns are easier to use,
-- however this module provides in-depth documentation.
--
module Network.TypedProtocol.Peer
  ( Peer (..)
  , PeerPipelined (..)
  , Receiver (..)
  , Outstanding
  , N (..)
  , Nat (Zero, Succ)
  , natToInt
  , unsafeIntToNat
  ) where

import           Data.Kind (Type)

import           Network.TypedProtocol.Core as Core

-- | A description of a peer that engages in a protocol.
--
-- The protocol describes what messages peers /may/ send or /must/ accept.
-- A particular peer implementation decides what to actually do within the
-- constraints of the protocol.
--
-- Peers engage in a protocol in either the client or server role. Of course
-- the client role can only interact with the serve role for the same protocol
-- and vice versa.
--
-- 'Peer' has several type arguments:
--
-- * the protocol itself;
-- * the client\/server role;
-- * whether the peer is using pipelining or not;
-- * the type level queue of future transitions not yet performed due to
--   pipelining;
-- * the current protocol state;
-- * the monad in which the peer operates (e.g. 'IO');
-- * the stm monad, (e.g. 'STM' or it can be left abstract if 'CollectSTM' is
--   not used);
-- * the type of the final result once the peer terminates.
--
-- For example:
--
-- > pingPongClientExample :: Int -> Peer PingPong AsClient    Pipelined Empty StIdle IO STM ()
-- > pingPongServerExample ::        Peer PingPong AsServer NonPipeliend Empty StIdle IO stm Int
--
-- The actions that a non pipelining peer can take are:
--
-- * to perform local monadic effects
-- * to terminate with a result (but only in a terminal protocol state)
-- * to send a message (but only in a protocol state in which we have agency)
-- * to wait to receive a message (but only in a protocol state in which the
--   other peer has agency)
--
-- In addition a pipelining peer can:
--
-- * pipeline a message, which requires upfront declaration at which state we
--   are continue.  This pushes the skipped transition to the back of the
--   pipelining queue.
-- * collect a response, which removes a transition from the front of the
--   queue.  It's worth to notice that this modifies the first element in the
--   queue, in particular it does not changes the queue length.
--   If there's no reply yet, collect allows to either block or continue,
--   possibly pipelining more messages.
-- * collect an identity transition (which removes the first element from the
--   queue).
-- * race between receiving a response and an stm transaction returning
--   a continuation.
--
-- The 'Yield', 'Await', 'Done', 'YieldPipelined', 'Collect',
-- constructors require to provide an evidence that the
-- peer has agency in the current state. The types guarantee that it is not
-- possible to supply incorrect evidence,  however you should use
-- 'Network.TypedProtocol.Peer.Client.Client' and
-- 'Network.TypedProtocol.Peer.Client.Server' pattern synonyms which provide
-- this evidence for you.
--
type Peer :: forall ps
          -> PeerRole
          -> IsPipelined
          -> ps
          -> (Type -> Type)
          -- ^ monad's kind
          -> Type
          -> Type
data Peer ps pr pl st m a where

  -- | Perform a local monadic effect and then continue.
  --
  -- Example:
  --
  -- > Effect $ do
  -- >   ...          -- actions in the monad
  -- >   return $ ... -- another Peer value
  --
  Effect
    :: forall ps pr pl st m a.
       m (Peer ps pr pl st m a)
    -- ^ monadic continuation
    ->    Peer ps pr pl st m a

  -- | Send a message to the other peer and then continue. This takes the
  -- message and the continuation. It also requires evidence that we have
  -- agency for this protocol state and thus are allowed to send messages.
  --
  -- Example:
  --
  -- > Yield ReflClientAgency MsgPing $ ...
  --
  Yield
    :: forall ps pr pl (st :: ps) (st' :: ps) m a.
       ( StateTokenI st
       , StateTokenI st'
       , ActiveState st
       , Outstanding pl ~ Z
       )
    => WeHaveAgencyProof pr st
    -- ^ agency proof
    -> Message ps st st'
    -- ^ protocol message
    -> Peer ps pr pl st' m a
    -- ^ continuation
    -> Peer ps pr pl st  m a

  -- | Waits to receive a message from the other peer and then continues.
  -- This takes the continuation that is supplied with the received message. It
  -- also requires evidence that the other peer has agency for this protocol
  -- state and thus we are expected to wait to receive messages.
  --
  -- Note that the continuation that gets supplied with the message must be
  -- prepared to deal with /any/ message that is allowed in /this/ protocol
  -- state. This is why the continuation /must/ be polymorphic in the target
  -- state of the message (the third type argument of 'Message').
  --
  -- Example:
  --
  -- > Await ReflClientAgency $ \msg ->
  -- > case msg of
  -- >   MsgDone -> ...
  -- >   MsgPing -> ...
  --
  Await
    :: forall ps pr pl (st :: ps) m a.
       ( StateTokenI st
       , ActiveState st
       , Outstanding pl ~ Z
       )
    => TheyHaveAgencyProof pr st
    -- ^ agency proof
    -> (forall (st' :: ps). Message ps st st'
        -> Peer ps pr pl st' m a)
    -- ^ continuation
    -> Peer     ps pr pl st  m a

  -- | Terminate with a result. A state token must be provided from the
  -- 'NobodyHasAgency' states, to show that this is a state in which we can
  -- terminate.
  --
  -- Example:
  --
  -- > Yield ReflClientAgency
  -- >        MsgDone
  -- >       (Done ReflNobodyAgency TokDone result)
  --
  Done
    :: forall ps pr pl (st :: ps) m a.
       ( StateTokenI st
       , StateAgency st ~ NobodyAgency
       , Outstanding pl ~ Z
       )
    => NobodyHasAgencyProof pr st
    -- ^ (no) agency proof
    -> a
    -- ^ returned value
    -> Peer ps pr pl st m a

  --
  -- Pipelining primitives
  --

  -- | Pipelined send which. Note that the continuation decides from which
  -- state we pipeline next message, and the gap is pushed at the back of
  -- the queue.
  --
  YieldPipelined
    :: forall ps pr (st :: ps) (st' :: ps) c n st'' m a.
       ( StateTokenI st
       , StateTokenI st'
       , ActiveState st
       )
    => WeHaveAgencyProof pr st
    -- ^ agency proof
    -> Message ps st st'
    -- ^ protocol message
    -> Receiver ps pr st' st'' m c
    -> Peer ps pr (Pipelined (S n) c) st'' m a
    -- ^ continuation
    -> Peer ps pr (Pipelined    n  c)   st   m a

  -- | Partially collect promised transition.
  --
  Collect
    :: forall ps pr c n st m a.
       ( StateTokenI st
       , ActiveState st
       )
    => Maybe (Peer ps pr (Pipelined (S n) c) st m a)
    -- ^ continuation, executed if no message has arrived so far
    -> (c ->  Peer ps pr (Pipelined    n  c)  st m a)
    -- ^ continuation
    -> Peer        ps pr (Pipelined (S n) c) st m a

deriving instance Functor m => Functor (Peer ps pr pl st m)


-- | Receiver 
type Receiver :: forall ps
              -> PeerRole
              -> ps
              -- ^ initial state
              -> ps
              -- ^ final state
              -> (Type -> Type)
              -- ^ monad
              -> Type
              -- ^ returned type by the receiver
              -> Type
data Receiver ps pr st stdone m c where

  ReceiverEffect :: m (Receiver ps pr st stdone m c)
                 ->    Receiver ps pr st stdone m c

  ReceiverDone   :: c -> Receiver ps pr stdone stdone m c

  ReceiverAwait  :: ( StateTokenI st
                    , ActiveState st
                    )
                 => TheyHaveAgencyProof pr st
                 -> (forall st'. Message ps st st'
                              -> Receiver ps pr st' stdone m c)
                 -> Receiver ps pr st stdone m c

deriving instance Functor m => Functor (Receiver ps pr st stdone m)

-- | A description of a peer that engages in a protocol in a pipelined fashion.
--
data PeerPipelined ps pr (st :: ps) m a where
    PeerPipelined :: { runPeerPipelined :: Peer ps pr (Pipelined Z c) st m a }
                  -> PeerPipelined ps pr st m a

deriving instance Functor m => Functor (PeerPipelined ps pr st m)
