{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}
-- TODO: the 'Functor' instance of 'Peer' is undecidable
{-# LANGUAGE UndecidableInstances     #-}

-- | Protocol EDSL.
--
-- __Note__: 'Network.TypedProtocol.Peer.Client.Client' and
-- 'Network.TypedProtocol.Peer.Server.Server' patterns are easier to use,
-- however this module provides in-depth documentation.
--
module Network.TypedProtocol.Peer (Peer (..)) where

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
-- The 'Yield', 'Await', 'Done', 'YieldPipelined', 'Collect', 'CollectDone',
-- 'CollectSTM' constructors require to provide an evidence that the
-- peer has agency in the current state. The types guarantee that it is not
-- possible to supply incorrect evidence,  however you should use
-- 'Network.TypedProtocol.Peer.Client.Client' and
-- 'Network.TypedProtocol.Peer.Client.Server' pattern synonyms which provide
-- this evidence for you.
--
type Peer :: forall ps
          -> PeerRole
          -> IsPipelined
          -> Queue ps
          -> ps
          -> (Type -> Type)
          -- ^ monad's kind
          -> (Type -> Type)
          -- ^ stm monad's kind, usually @'STM' m@
          -> Type
          -> Type
data Peer ps pr pl q st m stm a where

  -- | Perform a local monadic effect and then continue.
  --
  -- Example:
  --
  -- > Effect $ do
  -- >   ...          -- actions in the monad
  -- >   return $ ... -- another Peer value
  --
  Effect
    :: forall ps pr pl q st m stm a.
       m (Peer ps pr pl q st m stm a)
    -- ^ monadic continuation
    ->    Peer ps pr pl q st m stm a

  -- | Send a message to the other peer and then continue. This takes the
  -- message and the continuation. It also requires evidence that we have
  -- agency for this protocol state and thus are allowed to send messages.
  --
  -- Example:
  --
  -- > Yield ReflClientAgency MsgPing $ ...
  --
  Yield
    :: forall ps pr pl (st :: ps) (st' :: ps) m stm a.
       ( StateTokenI st
       , StateTokenI st'
       , ActiveState st
       )
    => WeHaveAgencyProof pr st
    -- ^ agency proof
    -> Message ps st st'
    -- ^ protocol message
    -> Peer ps pr pl Empty st' m stm a
    -- ^ continuation
    -> Peer ps pr pl Empty st  m stm a

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
    :: forall ps pr pl (st :: ps) m stm a.
       ( StateTokenI st
       , ActiveState st
       )
    => TheyHaveAgencyProof pr st
    -- ^ agency proof
    -> (forall (st' :: ps). Message ps st st'
        -> Peer ps pr pl Empty st' m stm a)
    -- ^ continuation
    -> Peer     ps pr pl Empty st  m stm a

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
    :: forall ps pr pl (st :: ps) m stm a.
       ( StateTokenI st
       , StateAgency st ~ NobodyAgency
       )
    => NobodyHasAgencyProof pr st
    -- ^ (no) agency proof
    -> a
    -- ^ returned value
    -> Peer ps pr pl Empty st m stm a

  --
  -- Pipelining primitives
  --

  -- | Pipelined send which. Note that the continuation decides from which
  -- state we pipeline next message, and the gap is pushed at the back of
  -- the queue.
  --
  YieldPipelined
    :: forall ps pr (st :: ps) (st' :: ps) q st'' m stm a.
       ( StateTokenI st
       , StateTokenI st'
       , ActiveState st
       )
    => WeHaveAgencyProof pr st
    -- ^ agency proof
    -> Message ps st st'
    -- ^ protocol message
    -> Peer ps pr Pipelined (q |> Tr st' st'') st'' m stm a
    -- ^ continuation
    -> Peer ps pr Pipelined  q                 st   m stm a

  -- | Partially collect promised transition.
  --
  Collect
    :: forall ps pr (st' :: ps) (st'' :: ps) q st m stm a.
       ( StateTokenI st'
       , ActiveState st'
       )
    => TheyHaveAgencyProof pr st'
    -- ^ agency proof
    -> Maybe (Peer ps pr Pipelined (Tr st' st'' <| q) st m stm a)
    -- ^ continuation, executed if no message has arrived so far
    -> (forall (stNext :: ps). Message ps st' stNext
        -> Peer ps pr Pipelined (Tr stNext st'' <| q) st m stm a)
    -- ^ continuation
    -> Peer     ps pr Pipelined (Tr st'    st'' <| q) st m stm a

  -- | Collect the identity transition.
  --
  -- 'CollectDone' allows to defer popping @Tr ps st st@ from the queue
  -- after a message is received (in 'Collect' callback), unlike 'Collect'
  -- which needs to know the transition type at compile time.
  --
  CollectDone
    :: forall ps pr (st :: ps) q (st' :: ps) m stm a.
       Peer ps pr Pipelined              q  st' m stm a
    -- ^ continuation
    -> Peer ps pr Pipelined (Tr st st <| q) st' m stm a

  -- The 'Peer' driver will race two transactions, the peer continuation versus
  -- next message.
  --
  -- Note: the driver, or interpreter if you wish, will build an stm
  -- transaction which will race between two events:
  --
  -- * received message
  -- * the given stm continuation
  --
  CollectSTM
    :: forall ps pr (st' :: ps) (st'' :: ps) q (st :: ps) m stm a.
       ( StateTokenI st'
       , ActiveState st'
       )
    => TheyHaveAgencyProof pr st'
    -- ^ agency proof
    -> stm (Peer ps pr Pipelined (Tr st' st'' <| q) st m stm a)
    -- ^ continuation, which is executed if it wins the race with the next
    -- message.
    -> (forall stNext. Message ps st' stNext
        -> Peer ps pr Pipelined (Tr stNext st'' <| q) st m stm a)
    -- ^ continuation
    -> Peer     ps pr Pipelined (Tr st'    st'' <| q) st m stm a

deriving instance (Functor m, Functor stm) => Functor (Peer ps (pr :: PeerRole) pl q (st :: ps) m stm)
