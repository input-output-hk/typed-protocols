{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE TypeOperators            #-}
-- need for 'Show' instance of 'ProtocolState'
{-# LANGUAGE UndecidableInstances     #-}


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
  , PeerRole (..)
  , SingPeerRole (..)
  , Agency (..)
  , SingAgency (..)
  , RelativeAgency (..)
  , Relative
  , ReflRelativeAgency (..)
  , FlipAgency
  , Pipelined (..)
  , Trans (..)
  , SingTrans (..)
  , ActiveAgency
  , ActiveAgency' (..)
  , IsActiveState (..)
  , ActiveState
  , notActiveState
    -- * Protocol proofs and tests
    -- $tests
    -- $lemmas
  , exclusionLemma_ClientAndServerHaveAgency
  , terminationLemma_1
  , terminationLemma_2
  , ReflNobodyHasAgency (..)
  ) where

import           Data.Kind (Constraint, Type)

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
-- send) and the other does not (it must only receive).
--
-- In the \"ping\/pong\" protocol example, the idle state is the one in which
-- the client can send a message, and the busy state is the one in which the
-- server must respond. Finally in the done state, neither peer can send any
-- further messages. This arrangement is defined as so:
--
-- >    -- still within the instance Protocol PingPong
-- >    type StateAgency StIdle = ClientAgency
-- >    type StateAgency StBusy = ServerAgency
-- >    type StateAgency StDone = NobodyAgency
--
-- In this simple protocol there is exactly one state in each category, but in
-- general for non-trivial protocols there may be several protocol states in
-- each category.
--
-- Furthermore we use singletons to provide term level reflection of type level
-- states.  One is required to provide singletons for all types of kind
-- 'PingPong'.  This is as simple as providing a GADT:
--
-- > data SingPingPong (st :: PingPong) where
-- >   SingIdle :: SingPingPong StIdle
-- >   SingBusy :: SingPingPong StBusy
-- >   SingDone :: SingPingPong StDone
--
-- together with 'Sing' and 'SingI' instances:
--
-- > type instance Sing = SingPingPong
-- > instance SingI StIdle where sing = SingIdle
-- > instance SingI StBusy where sing = SingBusy
-- > instance SingI StDone where sing = SingDone

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

-- | Singletons for 'PeerRole'.  We provide 'Sing' and 'SingI' instances from
-- the "singletons" package.
--
type SingPeerRole :: PeerRole -> Type
data SingPeerRole pr where
    SingAsClient :: SingPeerRole AsClient
    SingAsServer :: SingPeerRole AsServer

deriving instance Show (SingPeerRole pr)

type instance Sing = SingPeerRole
instance SingI AsClient where
    sing = SingAsClient
instance SingI AsServer where
    sing = SingAsServer

-- | A promoted data type which denotes three possible agencies a protocol
-- state might be assigned.
--
data Agency where
    -- | The client has agency.
    ClientAgency :: Agency

    -- | The server has agency.
    ServerAgency :: Agency

    -- | Nobody has agency, terminal state.
    NobodyAgency :: Agency

type SingAgency :: Agency -> Type
data SingAgency a where
    SingClientAgency :: SingAgency ClientAgency
    SingServerAgency :: SingAgency ServerAgency
    SingNobodyAgency :: SingAgency NobodyAgency

deriving instance Show (SingAgency a)

type instance Sing = SingAgency
instance SingI ClientAgency where
    sing = SingClientAgency
instance SingI ServerAgency where
    sing = SingServerAgency
instance SingI NobodyAgency where
    sing = SingNobodyAgency

-- | A promoted data type which indicates the effective agency (which is
-- relative to current role).
--
data RelativeAgency where
    WeHaveAgency    :: RelativeAgency
    TheyHaveAgency  :: RelativeAgency
    NobodyHasAgency :: RelativeAgency


-- | Compute effective agency with respect to the peer role, for client role,
-- agency is preserved, while for the server role it is flipped.
--
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
-- @r :~: r'@, where both @r@ and @r'@ have kind 'RelativeAgency'.
--
type ReflRelativeAgency :: Agency -> RelativeAgency -> RelativeAgency -> Type
data ReflRelativeAgency a r r' where
    ReflClientAgency :: ReflRelativeAgency ClientAgency r r
    ReflServerAgency :: ReflRelativeAgency ServerAgency r r
    ReflNobodyAgency :: ReflRelativeAgency NobodyAgency r r

-- $lemmas
--
-- The 'connect' proof rely on lemmas about the protocol. Specifically they
-- rely on the property that each protocol state is labelled with the agency of
-- one peer or the other, or neither, but never both.  This property is true by
-- construction, since we use a type family 'StateAgency' which maps states to
-- agencies, however we still need an evince that cases where both peer have
-- the agency or neither of them has it can be eliminated.
--
-- The provided lemmas are structured as proofs by contradiction, e.g. stating
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
-- Each protocol consists of four components:
--
-- * The protocol itself, which is also expected to be the kind of the types
--   of the protocol states. The class is indexed on the protocol itself;
-- * the protocol messages;
-- * a type level map from the protocol states to agency: in each state either
--   client or server or nobody has the agency.
-- * a singleton type for the protocol states (e.g. `Sing` type family
--   instance), together with 'SingI' instances.
--
-- It is required provide 'Sing' type family instance as well as 'SingI'
-- instances for all protocol states.  These singletons allow one to pattern
-- match on the state, which is useful when defining codecs, or providing
-- informative error messages, however they are not necessary for proving
-- correctness of the protocol.
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

type ActiveAgency' :: ps -> Agency -> Type
data ActiveAgency' st agency where
  ClientHasAgency :: StateAgency st ~ ClientAgency
                  => ActiveAgency' st ClientAgency
  ServerHasAgency :: StateAgency st ~ ServerAgency
                  => ActiveAgency' st ServerAgency

deriving instance Show (ActiveAgency' st agency)

type ActiveAgency :: ps -> Type
type ActiveAgency st = ActiveAgency' st (StateAgency st)

-- | A type class which restricts states to ones that have `ClientAgency` or
-- `ServerAgency`, excluding `NobodyAgency`.
--
-- One can use `notActive' to eliminate cases for which both @'IsActiveState'
-- st@ is in scope and for which we have an evidence that the state is not
-- active (i.e. a singleton).  This is useful when writing a 'Codec'.
--
class IsActiveState st (agency :: Agency) where
  activeAgency :: ActiveAgency' st agency

instance ClientAgency ~ StateAgency st
      => IsActiveState st ClientAgency where
  activeAgency = ClientHasAgency
instance ServerAgency ~ StateAgency st
      => IsActiveState st ServerAgency where
  activeAgency = ServerHasAgency

type ActiveState :: ps -> Constraint
type ActiveState st = IsActiveState st (StateAgency st)


-- | This is useful function to eliminate cases where the `ActiveState st` is
-- provided but we are given a state in which neither side has agency
-- (`NobodyAgency`).  For example when writing a codec, we only need to encode
-- / decode messages which are in active states, but to make such functions
-- total, `notActiveState` needs to be used to eliminate the states in which
-- nobody has agency.
--
-- A good analogy for this function is @'Data.Void.absurd' :: 'Void' -> a@.
--
notActiveState :: forall ps (st :: ps).
                  StateAgency st ~ NobodyAgency
               => ActiveState st
               => Sing st
               -> forall a. a
notActiveState (_ :: Sing st) =
  case activeAgency :: ActiveAgency st of {}


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
-- equal then nobody has agency.  In particular this lemma excludes the
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
-- Note: this could be proven the same way 'terminationLemma_1' is proved, but
-- instead we use two lemmas to reduce the assumptions (arguments) and we apply
-- 'terminationLemma_1'.
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


-- | Promoted data type which indicates if 'Peer' is used in
-- pipelined mode or not.
--
data Pipelined where
    -- | Pipelined peer which is using `c :: Type` for collecting responses
    -- from a pipelined messages.
    Pipelined    :: Type -> Pipelined

    -- | Non-pipelined peer.
    NonPipelined :: Pipelined
