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
-- __Note__: You should use pattern synonyms exposed in
-- "Network.TypedProtocol.Peer.Client" and "Network.TypedProtocol.Peer.Server",
-- however here we provide in-depth documentation.
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
-- * whether the peer is using pipelining or not, if pipelined it holds the
--   depth of pipelining and a type used to collect data from pipelined
--   transitions;
-- * the current protocol state;
-- * the monad in which the peer operates (e.g. 'IO');
-- * the type of the final result once the peer terminates.
--
-- For example:
--
-- > pingPongClientExample :: Peer PingPong AsClient (Pipelined Z Int) StIdle IO ()
-- > pingPongServerExample :: Peer PingPong AsServer NonPipeliend      StIdle IO Int
--
-- The actions that a non-pipelining peer can take are:
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
--   continue at and passing a receiver which will run in parallel.  When
--   receiver terminates it pushes the result into the pipelining queue.
-- * collect a response from the pipelining queue.
--
-- The 'Yield', 'Await', 'Done', 'YieldPipelined', 'Collect',
-- constructors require to provide an evidence that the
-- peer has agency in the current state. The types guarantee that it is not
-- possible to supply incorrect evidence,  however the
-- pattern synonyms exposed in "Network.TypedProtocol.Peer.Client" and
-- "Network.TypedProtocol.Peer.Client" supply this evidence for you, and hence
-- are easier to use and let you avoid some kinds of type errors.
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

  -- | Pipelined send.  We statically decide from which state we continue (the
  -- `st''` state here), the gap (between `st'` and `st''`) must be fulfilled
  -- by 'Receiver' which runs will run in parallel.
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
    -- ^ receiver
    -> Peer ps pr (Pipelined (S n) c) st'' m a
    -- ^ continuation from state `st''`
    -> Peer ps pr (Pipelined    n  c)   st   m a

  -- | Collect results returned by a `Receiver`.  Results are collected in the
  -- first-in-first-out way.
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


-- | Receiver.  It is limited to only awaiting for messages and running monadic
-- computations.  This means that one can only pipeline messages if they can be
-- connected by state transitions which all have remote agency.
--
-- The receiver runs in parallel, see `runPipelinedPeerWithDriver`.  This makes
-- pipelining quite effective, since the receiver callbacks are called in
-- a separate thread which can effectively use CPU cache and can avoids
-- unnecessary context switches.
--
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

  -- | Execute a monadic computation.
  --
  ReceiverEffect :: m (Receiver ps pr st stdone m c)
                 ->    Receiver ps pr st stdone m c

  -- | Return value.
  --
  ReceiverDone   :: c -> Receiver ps pr stdone stdone m c

  -- | Await for for a remote transition.
  --
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
-- This type is useful for wrapping pipelined peers to hide information which
-- is only relevant in peer lift.  It is expected by
-- `Network.TypedProtocol.Driver.runPeerPipelinedWithDriver`.
--
data PeerPipelined ps pr (st :: ps) m a where
    PeerPipelined :: { runPeerPipelined :: Peer ps pr (Pipelined Z c) st m a }
                  -> PeerPipelined ps pr st m a

deriving instance Functor m => Functor (PeerPipelined ps pr st m)
