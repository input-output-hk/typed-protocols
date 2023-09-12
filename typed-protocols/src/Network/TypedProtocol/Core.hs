{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE TypeOperators            #-}


-- | This module defines the core of the typed protocol framework.
--

module Network.TypedProtocol.Core
  ( -- * Introduction
    -- $intro
    -- * Defining protocols
    -- $defining
    Protocol (..)
    -- $lemmas
    -- * Engaging in protocols
    -- $using
  , PeerRole (..)
  , SingPeerRole (..)
  , Agency (..)
  , RelativeAgency (..)
  , Relative
  , ReflRelativeAgency (..)
  , FlipAgency
  , Pipelined (..)
  , Trans (..)
  , SingTrans (..)
  , Queue (..)
  , type (<|)
  , type (|>)
  , SingQueue (..)
  , (|>)
  , Peer (..)
    -- * Protocol proofs and tests
    -- $tests
    -- $lemmas
  , exclusionLemma_ClientAndServerHaveAgency
  , terminationLemma_1
  , terminationLemma_2
  , ReflNobodyHasAgency (..)
  ) where

import           Data.Kind (Type)

import           Data.Singletons

-- $intro
-- A typed protocol between two peers is defined via a state machine: a
-- collection of protocol states and protocol messages which are transitions
-- between those states.
--
-- Start from the idea that a protocol is some language of messages sent
-- between two peers. To specify a protocol is to describe what possible
-- sequences of messages are valid. One simple but still relatively expressive
-- way to do this is via a state machine: starting from some initial state,
-- all possible paths through the state machine gives the set of valid protocol
-- traces. This then dictates what a peer participating in a protocol may
-- produce and what it must accept.
--
-- In this style we have a fixed number of states and in each state there is
-- some number of valid messages that move us on to the next state. This can be
-- illustrated as a graph, which can be a helpful form of documentation.
--
-- We further constrain this idea by saying that the two peers will use the
-- same state machine and change states in lock-step by sending\/receiving
-- messages. In this approach, for each protocol state, the description
-- dictates which peer has the agency to choose to send a message, while
-- correspondingly the other must be prepared to receive the message.
--
-- The views of the two peers are dual. In each state one peer can send any
-- message that is valid for the current protocol state while the other
-- must be prepared to receive any valid message for current protocol state.
--
-- We can also have terminal protocol states in which neither peer has agency.
--
-- So part of the protocol description is to label each protocol state with
-- the peer that has the agency in that state, or none for terminal states.
-- We use the labels \"client\" and \"server\" for the two peers, but they are
-- in fact symmetric.


-- $defining
--
-- The 'Protocol' type class bundles up all the requirements for a typed
-- protocol, which are in fact all type level constructs. Defining a new
-- protocol and making it an instance of the 'Protocol' class requires the
-- following language extensions:
--
-- > {-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}
--
-- The type class itself is indexed on a protocol \"tag\" type. This type
-- does double duty as the /kind/ of the /types/ of the protocol states.

-- We will use as a running example a simple \"ping\/pong\" protocol. (You can
-- see the example in full in "Network.TypedProtocol.PingPong.Type".) In this
-- example protocol the client sends a ping message and the serve must respond
-- with a pong message. The client can also terminate the protocol. So modelled
-- as a state machine this protocol has three states, the one in which the
-- client can send a ping or terminate message, the one in which the server
-- must send a pong, and the terminal state where neither can send anything.
-- We somewhat arbitrarily choose label these protocol states as \"idle\"
-- \"busy\" and \"done\".
--
-- For this ping pong example the protocol tag and the protocol state types
-- would be defined (via promoted data kinds) as:
--
-- > data PingPong where
-- >   StIdle :: PingPong
-- >   StBusy :: PingPong
-- >   StDone :: PingPong
--
-- We use @DataKinds@ promotion here so @StIdle@, @StBusy@ and @StDone@ are
-- /types/ (of /kind/ @PingPong@) representing the three states in this
-- protocol's state machine. @PingPong@ itself is both the kind of these types
-- and is also the tag for protocol. We only ever use these as types, via the
-- @DataKinds@ promotion, never as value level data constructors.
--
-- Having defined our protocol tag and states we can instantiate the 'Protocol'
-- type class and fill out the other details.
--
-- The protocol must define what its messages are. These form the state
-- transitions in the protocol state machine. Each transition specifies a
-- \"from\" and \"to\" state as type parameters. This of course determines in
-- which protocol states each message can appear.
--
-- In the \"ping\/pong\" protocol example, the messages are of course ping and
-- pong, which transition between the two main states. There is also a done
-- message that moves the system into a terminal state.
--
-- > instance Protocol PingPong where
-- >   data Message PingPong from to where
-- >     MsgPing :: Message PingPong StIdle StBusy
-- >     MsgPong :: Message PingPong StBusy StIdle
-- >     MsgDone :: Message PingPong StIdle StDone
--
-- This says that in the idle state a ping message takes us to the busy state,
-- while a pong message takes us back to idle. Also in the idle state a done
-- message takes us to the done state.
--
-- It is not required that protocols have any terminal states or corresponding
-- transitions, as in this example, but it is often useful and it aids testing
-- to have protocols that terminate cleanly as it allows them to return a
-- result.
--
-- As described above, this style of protocol gives agency to only one peer at
-- once. That is, in each protocol state, one peer has agency (the ability to
-- send) and the other does not (it must only receive). The three associated
-- data families ('ClientHasAgency', 'ServerHasAgency' and 'NobodyHasAgency')
-- define which peer has agency for each state.
--
-- In the \"ping\/pong\" protocol example, the idle state is the one in which
-- the client can send a message, and the busy state is the one in which the
-- server must respond. Finally in the done state, neither peer can send any
-- further messages. This arrangement is defined as so:
--
-- >    -- still within the instance Protocol PingPong
-- >    data ClientHasAgency st where
-- >      TokIdle :: ClientHasAgency StIdle
-- >
-- >    data ServerHasAgency st where
-- >      TokBusy :: ServerHasAgency StBusy
-- >
-- >    data NobodyHasAgency st where
-- >      TokDone :: NobodyHasAgency StDone
--
-- In this simple protocol there is exactly one state in each category, but in
-- general for non-trivial protocols there may be several protocol states in
-- each category.
--

-- $tests
-- The mechanism for labelling each protocol state with the agency does not
-- automatically prevent mislabelling, ie giving conflicting labels to a
-- single state. It does in fact prevent forgetting to label states in the
-- sense that it would not be possible to write protocol peers that make
-- progress having entered these unlabelled states.
--
-- This partition property is however crucial for the framework's guarantees.
-- The "Network.TypedProtocol.Proofs" module provides a way to guarantee for
-- each protocol that this property is not violated. It also provides utilities
-- helpful for testing protocols.


-- | Types for client and server peer roles. As protocol can be viewed from
-- either client or server side.
--
-- Note that technically \"client\" and \"server\" are arbitrary labels. The
-- framework is completely symmetric between the two peers.
--
-- This definition is only used as promoted types and kinds, never as values.
--
data PeerRole = AsClient | AsServer

type SingPeerRole :: PeerRole -> Type
data SingPeerRole pr where
    SingAsClient :: SingPeerRole AsClient
    SingAsServer :: SingPeerRole AsServer

type instance Sing = SingPeerRole
instance SingI AsClient where
    sing = SingAsClient
instance SingI AsServer where
    sing = SingAsServer

data Agency where
    ClientAgency :: Agency
    ServerAgency :: Agency
    NobodyAgency :: Agency


data RelativeAgency where
    WeHaveAgency    :: RelativeAgency
    TheyHaveAgency  :: RelativeAgency
    NobodyHasAgency :: RelativeAgency


type        Relative :: PeerRole -> Agency -> RelativeAgency
type family Relative  pr a where
  Relative AsClient ClientAgency = WeHaveAgency
  Relative AsClient ServerAgency = TheyHaveAgency
  Relative AsClient NobodyAgency = NobodyHasAgency
  Relative AsServer ClientAgency = TheyHaveAgency
  Relative AsServer ServerAgency = WeHaveAgency
  Relative AsServer NobodyAgency = NobodyHasAgency


-- | Type equality for 'RelativeAgency' which also carries information about
-- agency.  It is isomorphic to a product of 'Agency' singleton and
-- @'RelativeAgency' :~: 'RelativeAgency'@.
--
type ReflRelativeAgency :: Agency -> RelativeAgency -> RelativeAgency -> Type
data ReflRelativeAgency a r r' where
    ReflClientAgency :: ReflRelativeAgency ClientAgency r r
    ReflServerAgency :: ReflRelativeAgency ServerAgency r r
    ReflNobodyAgency :: ReflRelativeAgency NobodyAgency r r


-- $lemmas
--
-- The 'connect' and 'connectPipelined' proofs rely on lemmas about the
-- protocol. Specifically they rely on the property that each protocol state
-- is labelled with the agency of one peer or the other, or neither, but never
-- both. Or to put it another way, the protocol states should be partitioned
-- into those with agency for one peer, or the other or neither.
--
-- The way the labelling is encoded does not automatically enforce this
-- property. It is technically possible to set up the labelling for a protocol
-- so that one state is labelled as having both peers with agency, or declaring
-- simultaneously that one peer has agency and that neither peer has agency
-- in a particular state.
--
-- So the overall proofs rely on lemmas that say that the labelling has been
-- done correctly. This type bundles up those three lemmas.
--
-- Specifically proofs that it is impossible for a protocol state to have:
--
-- * client having agency and server having agency
-- * client having agency and nobody having agency
-- * server having agency and nobody having agency
--
-- These lemmas are structured as proofs by contradiction, e.g. stating
-- \"if the client and the server have agency for this state then it leads to
-- contradiction\". Contradiction is represented as the 'Void' type that has
-- no values except ⊥.
--
-- For example for the ping\/pong protocol, it has three states, and if we set
-- up the labelling correctly we have:
--
-- > data PingPong where
-- >   StIdle :: PingPong
-- >   StBusy :: PingPong
-- >   StDone :: PingPong
-- >
-- > instance Protocol PingPong where
-- >     data Message PingPong st st' where
-- >       MsgPing :: Message PingPong StIdle StBusy
-- >       MsgPong :: Message PingPong StBusy StIdle
-- >       MsgDone :: Message PingPong StIdle StDone
-- >
-- >     data TokState PingPong st where
-- >       TokIdle :: TokState PingPong StIdle
-- >       TokBusy :: TokState PingPong StBusy
-- >       TokDone :: TokState PingPong StDone
-- >
-- >     type StateAgency StIdle = ClientAgency
-- >     type StateAgency StBusy = ServerAgency
-- >     type StateAgency StDone = NobodyAgency
--
-- The framework provides proofs which excludes that the client and server have
-- agency at the same time.
--
-- * 'exclusionLemma_ClientAndServerHaveAgency',
-- * 'terminationLemma_1',
-- * 'terminationLemma_2'.
--
-- These lemmas are proven for all protocols.
--

-- | The protocol type class bundles up all the requirements for a typed
-- protocol.
--
-- Each protocol consists of three things:
--
-- * The protocol itself, which is also expected to be the kind of the types
--   of the protocol states. The class is indexed on the protocol itself.
-- * The protocol messages.
-- * The partition of the protocol states into those in which the client has
--   agency, or the server has agency, or neither have agency.
--
-- The labelling of each protocol state with the peer that has agency in that
-- state is done by giving a definition to the type family
-- 'StateAgency'.  It is required provide 'Sing' type family instance as well
-- as 'SingI' instances for all protocol states.  These singletons allow one to
-- pattern match on the state, which is useful when defining codecs, or
-- providing informative error messages.
--
class Protocol ps where

  -- | The messages for this protocol. It is expected to be a GADT that is
  -- indexed by the @from@ and @to@ protocol states. That is the protocol state
  -- the message transitions from, and the protocol state it transitions into.
  -- These are the edges of the protocol state transition system.
  --
  data Message ps (st :: ps) (st' :: ps)

  -- | Associate an 'Agency' for each state.
  --
  type StateAgency (st :: ps) :: Agency


-- | A type function to flip the client and server roles.
--
type        FlipAgency :: PeerRole -> PeerRole
type family FlipAgency pr where
  FlipAgency AsClient = AsServer
  FlipAgency AsServer = AsClient


-- | An evidence that both relative agencies are equal to 'NobodyHasAgency'.
--
type ReflNobodyHasAgency :: RelativeAgency -> RelativeAgency -> Type
data ReflNobodyHasAgency ra ra' where
     ReflNobodyHasAgency :: ReflNobodyHasAgency
                                NobodyHasAgency
                                NobodyHasAgency


-- | A proof that if both @Relative pr a@ and @Relative (FlipAgency pr) a@ are
-- equal then nobody has agency.  In particual this lemma excludes the
-- possibility that client and server has agency at the same state.
--
exclusionLemma_ClientAndServerHaveAgency
  :: forall (pr :: PeerRole) (a :: Agency)
            (ra  :: RelativeAgency).
     SingPeerRole pr
  -> ReflRelativeAgency a ra (Relative             pr  a)
  -> ReflRelativeAgency a ra (Relative (FlipAgency pr) a)
  -> ReflNobodyHasAgency     (Relative             pr  a)
                             (Relative (FlipAgency pr) a)
exclusionLemma_ClientAndServerHaveAgency
  SingAsClient ReflNobodyAgency ReflNobodyAgency = ReflNobodyHasAgency
exclusionLemma_ClientAndServerHaveAgency
  SingAsServer ReflNobodyAgency ReflNobodyAgency = ReflNobodyHasAgency

exclusionLemma_ClientAndServerHaveAgency
  SingAsClient ReflClientAgency x        = case x of {}
exclusionLemma_ClientAndServerHaveAgency
  SingAsServer ReflClientAgency x        = case x of {}
exclusionLemma_ClientAndServerHaveAgency
  SingAsClient ReflServerAgency x        = case x of {}
exclusionLemma_ClientAndServerHaveAgency
  SingAsServer ReflServerAgency x        = case x of {}


-- | A proof that if one side has terminated, then the other side terminated as
-- well.
--
terminationLemma_1
  :: SingPeerRole pr
  -> ReflRelativeAgency a ra              (Relative             pr  a)
  -> ReflRelativeAgency a NobodyHasAgency (Relative (FlipAgency pr) a)
  -> ReflNobodyHasAgency                  (Relative             pr  a)
                                          (Relative (FlipAgency pr) a)
terminationLemma_1
  SingAsClient ReflNobodyAgency ReflNobodyAgency = ReflNobodyHasAgency
terminationLemma_1
  SingAsServer ReflNobodyAgency ReflNobodyAgency = ReflNobodyHasAgency
terminationLemma_1 SingAsClient ReflClientAgency x = case x of {}
terminationLemma_1 SingAsClient ReflServerAgency x = case x of {}
terminationLemma_1 SingAsServer ReflClientAgency x = case x of {}
terminationLemma_1 SingAsServer ReflServerAgency x = case x of {}


-- | Internal; only need to formulate auxiliary lemmas in the proof of
-- 'terminationLemma_2'.
--
type        FlipRelAgency :: RelativeAgency -> RelativeAgency
type family FlipRelAgency ra where
  FlipRelAgency WeHaveAgency    = TheyHaveAgency
  FlipRelAgency TheyHaveAgency  = WeHaveAgency
  FlipRelAgency NobodyHasAgency = NobodyHasAgency


-- | Similar to 'terminationLemma_1'.
--
-- Note: this could be proven in the same way 'terminationLemma_1' is proved,
-- but instead we use two lemmas to reduce the assumptions (arguments) and
-- we apply 'terminationLemma_1'.
--
terminationLemma_2
  :: SingPeerRole pr
  -> ReflRelativeAgency a ra              (Relative (FlipAgency pr) a)
  -> ReflRelativeAgency a NobodyHasAgency (Relative             pr  a)
  -> ReflNobodyHasAgency                  (Relative (FlipAgency pr) a)
                                          (Relative             pr  a)

terminationLemma_2 singPeerRole refl refl' =
    case terminationLemma_1 singPeerRole
                       (lemma_flip  singPeerRole refl)
                       (lemma_flip' singPeerRole refl')
    of x@ReflNobodyHasAgency -> x
    -- note: if we'd swap arguments of the returned @ReflNobodyHasAgency@ type,
    -- we wouldn't need to pattern match on the result.  But in this form the
    -- lemma is a symmetric version of 'terminationLemma_1'.
  where
    lemma_flip
      :: SingPeerRole pr
      -> ReflRelativeAgency a                ra  (Relative (FlipAgency pr) a)
      -> ReflRelativeAgency a (FlipRelAgency ra) (Relative             pr  a)

    lemma_flip'
      :: SingPeerRole pr
      -> ReflRelativeAgency a                ra  (Relative             pr  a)
      -> ReflRelativeAgency a (FlipRelAgency ra) (Relative (FlipAgency pr) a)

    -- both lemmas are identity functions:
    lemma_flip  SingAsClient ReflClientAgency = ReflClientAgency
    lemma_flip  SingAsClient ReflServerAgency = ReflServerAgency
    lemma_flip  SingAsClient ReflNobodyAgency = ReflNobodyAgency
    lemma_flip  SingAsServer ReflClientAgency = ReflClientAgency
    lemma_flip  SingAsServer ReflServerAgency = ReflServerAgency
    lemma_flip  SingAsServer ReflNobodyAgency = ReflNobodyAgency

    lemma_flip' SingAsClient ReflClientAgency = ReflClientAgency
    lemma_flip' SingAsClient ReflServerAgency = ReflServerAgency
    lemma_flip' SingAsClient ReflNobodyAgency = ReflNobodyAgency
    lemma_flip' SingAsServer ReflClientAgency = ReflClientAgency
    lemma_flip' SingAsServer ReflServerAgency = ReflServerAgency
    lemma_flip' SingAsServer ReflNobodyAgency = ReflNobodyAgency


-- | Transition kind.
--
data Trans ps where
    Tr :: forall ps. ps -> ps -> Trans ps


-- | Singleton for @'Trans' ps@ kind.
--
type SingTrans :: Trans ps -> Type
data SingTrans tr where
    SingTr :: forall ps (st :: ps) (st' :: ps).
              SingTrans (Tr st st')

-- | Queue kind.  The type level queue is used to push pipelined transitions
-- and pop them from its other side when one is requesting to collect pipelined
-- results.
--
data Queue ps where
  Empty :: Queue ps
  Cons  :: Trans ps -> Queue ps -> Queue ps

-- | Cons type alias
--
type  (<|) :: Trans ps -> Queue ps -> Queue ps
type a <| as = Cons a as
infixr 5 <|

-- | Snoc operator
--
type (|>) :: Queue ps -> Trans ps -> Queue ps
type family as |> b where
     Empty     |> b = Cons b Empty
     (a <| as) |> b = a <| (as |> b)
infixr 5 |>

-- | Singleton data type which allows to track the types of kind
-- @'Queue' ps@.
--
type SingQueue :: Queue ps -> Type
data SingQueue q where
    SingEmpty :: SingQueue Empty
    SingCons  :: forall ps (st :: ps) (st' :: ps) (q :: Queue ps).
                 SingQueue q
              -> SingQueue (Tr st st' <| q)

-- | Term level '|>' (snoc).
--
(|>) :: forall ps (st :: ps) (st' :: ps) (q :: Queue ps).
        SingQueue q
     -> SingTrans (Tr st st')
     -> SingQueue (q |> Tr st st')
(|>)  SingEmpty   !_   = SingCons SingEmpty
(|>) (SingCons q) !str = SingCons (q |> str)

-- | Promoted data type which indicates if 'Peer' is used in
-- pipelined mode or not.
--
data Pipelined = NonPipelined | Pipelined


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
-- *.the current protocol state;
-- * the monad in which the peer operates; and
-- * the type of any final result once the peer terminates.
--
-- For example:
--
-- > pingPongClientExample :: Int -> Peer PingPong AsClient StIdle m ()
-- > pingPongServerExample ::        Peer PingPong AsServer StIdle m Int
--
-- The actions that a peer can take are:
--
-- * to perform local monadic effects
-- * to terminate with a result (but only in a terminal protocol state)
-- * to send a message (but only in a protocol state in which we have agency)
-- * to wait to receive a message (but only in a protocol state in which the
--   other peer has agency)
--
-- The 'Yield', 'Await' and 'Done' constructors require to provide an evidence
-- that the appropriate peer has agency.  This information is suplied using
-- one of the constructors of 'ReflRelativeAgency'.
--
-- While this evidence must be provided, the types guarantee that it is not
-- possible to supply incorrect evidence.  The
-- 'Network.TypedProtocol.Peer.Client' or 'Network.TypedProtocol.Peer.Server'
-- pattern synonyms provide this evidence automatically.
--
type Peer :: forall ps
          -> PeerRole
          -> Pipelined
          -> Queue ps
          -> ps
          -> (Type -> Type)
          -- ^ monad's kind
          -> Type
          -> Type
data Peer ps pr pl q st m a where

  -- | Perform a local monadic effect and then continue.
  --
  -- Example:
  --
  -- > Effect $ do
  -- >   ...          -- actions in the monad
  -- >   return $ ... -- another Peer value
  --
  Effect
    :: m (Peer ps pr pl q st m a)
    -- ^ monadic continuation
    ->    Peer ps pr pl q st m a

  -- | Send a message to the other peer and then continue. This takes the
  -- message and the continuation. It also requires evidence that we have
  -- agency for this protocol state and thus are allowed to send messages.
  --
  -- Example:
  --
  -- > Yield ReflClientAgency MsgPing $ ...
  --
  Yield
    :: SingI st
    => (ReflRelativeAgency (StateAgency st)
                            WeHaveAgency
                           (Relative pr (StateAgency st)))
    -- ^ agency singleton
    -> Message ps st st'
    -- ^ protocol message
    -> Peer ps pr pl Empty st' m a
    -- ^ continuation
    -> Peer ps pr pl Empty st  m a

  -- | Waits to receive a message from the other peer and then continues.
  -- This takes the the continuation that is supplied with the received
  -- message. It also requires evidence that the other peer has agency for
  -- this protocol state and thus we are expected to wait to receive messages.
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
    :: SingI st
    => (ReflRelativeAgency (StateAgency st)
                            TheyHaveAgency
                           (Relative pr (StateAgency st)))
    -- ^ agency singleton
    -> (forall st'. Message ps st st'
        -> Peer ps pr pl Empty st' m a)
    -- ^ continuation
    -> Peer     ps pr pl Empty st  m a

  -- | Terminate with a result. A state token must be provided from the
  -- 'NobodyHasAgency' states, so show that this is a state in which we can
  -- terminate.
  --
  -- Example:
  --
  -- > Yield ReflClientAgency
  -- >        MsgDone
  -- >       (Done ReflNobodyAgency TokDone result)
  --
  Done
    :: SingI st
    => (ReflRelativeAgency (StateAgency st)
                            NobodyHasAgency
                           (Relative pr (StateAgency st)))
    -- ^ (no) agency singleton
    -> a
    -- ^ returned value
    -> Peer ps pr pl Empty st m a

  --
  -- Pipelining primitives
  --

  -- | Pipelined send which. Note that the continuation decides from which
  -- state we pipeline next message, and the gap is pushed at the back of
  -- the queue.
  --
  YieldPipelined
    :: (SingI st, SingI st')
    => (ReflRelativeAgency (StateAgency st)
                            WeHaveAgency
                           (Relative pr (StateAgency st)))
    -- ^ agency singleton
    -> Message ps st st'
    -- ^ protocol message
    -> Peer ps pr 'Pipelined (q |> Tr st' st'') st'' m a
    -- ^ continuation
    -> Peer ps pr 'Pipelined  q                 st   m a

  -- | Partially collect promised transition.
  --
  Collect
    :: (SingI st')
    => (ReflRelativeAgency (StateAgency st')
                            TheyHaveAgency
                           (Relative pr (StateAgency st')))
    -- ^ agency singleton
    -> Maybe (Peer ps pr 'Pipelined (Tr st' st'' <| q) st m a)
    -- ^ continuation, executed if no message has arrived so far
    -> (forall stNext. Message ps st' stNext
        -> Peer ps pr 'Pipelined (Tr stNext st'' <| q) st m a)
    -- ^ continuation
    -> Peer     ps pr 'Pipelined (Tr st'    st'' <| q) st m a

  -- | Collect the identity transition.
  --
  -- 'CollectDone' allows to defer popping @Tr ps st st@ from the queue
  -- after a message is received (in 'Collect' callback), unlike 'Collect'
  -- which needs to know the transition type at compile time.
  --
  CollectDone
    :: Peer ps pr 'Pipelined              q  st m a
    -- ^ continuation
    -> Peer ps pr 'Pipelined (Tr st st <| q) st m a


deriving instance Functor m => Functor (Peer ps (pr :: PeerRole) pl q (st :: ps) m)
