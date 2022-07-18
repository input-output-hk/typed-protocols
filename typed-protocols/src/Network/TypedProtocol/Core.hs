{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
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
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE TypeInType               #-}
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
  , WeHaveAgencyProof
  , TheyHaveAgencyProof
  , NobodyHasAgencyProof
  , FlipAgency
  , IsPipelined (..)
  , Transition (..)
  , SingTrans (..)
  , Queue (..)
  , type (<|)
  , type (|>)
  , SingQueueF (..)
  , (|>)
  , ActiveAgency
  , ActiveAgency' (..)
  , IsActiveState (..)
  , ActiveState
  , notActiveState
    -- * Utils
  , stateToken
  ) where

import           Data.Kind (Constraint, Type)
import           Data.Type.Queue

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

-- | Type of the proof that we have the agency.
--
-- 'ReflClientAgency' has this type only iff `'StateAgency' st ~ 'ClientAgency'`
-- and `pr ~ 'AsClient'`.
--
-- 'ReflServerAgency' has this type only iff `'StateAgency' st ~ 'ServerAgency'`
-- and `pr ~ 'AsServer'`
--
type WeHaveAgencyProof :: PeerRole -> ps -> Type
type WeHaveAgencyProof pr st = ReflRelativeAgency
                                 (StateAgency st)
                                  WeHaveAgency
                                 (Relative pr (StateAgency st))

-- | Type of the proof that the remote side has the agency.
--
-- 'ReflClientAgency' has this type only iff `'StateAgency' st ~ 'ClientAgency'`
-- and `pr ~ 'AsServer'`.
--
-- 'ReflServerAgency' has this type only iff `'StateAgency' st ~ 'ServerAgency'`
-- and `pr ~ 'AsClient'`
--
type TheyHaveAgencyProof :: PeerRole -> ps -> Type
type TheyHaveAgencyProof pr st = ReflRelativeAgency
                                   (StateAgency st)
                                    TheyHaveAgency
                                   (Relative pr (StateAgency st))


-- | Type of the proof that nobody has agency in this state.
--
-- Only 'ReflNobodyAgency' can fulfil the proof obligation.
--
type NobodyHasAgencyProof :: PeerRole -> ps -> Type
type NobodyHasAgencyProof pr st = ReflRelativeAgency (StateAgency st)
                                                      NobodyHasAgency
                                                     (Relative pr (StateAgency st))

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
-- These lemmas are proven for all protocols.  These proofs rely on @EmptyCase@
-- extension.  To get this completeness checking it is important to compile
-- modules containing these lemmas with @-Wincomplete-patterns@, which is
-- implied by @-Wall@.
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

  -- | A type alias for protocol state token, e.g. term level representation of
  -- type level state (also known as singleton).
  --
  type StateToken :: ps -> Type

-- | An alias for 'sing'.
--
stateToken :: (SingI st, Sing st ~ StateToken st) => StateToken st
stateToken = sing

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

-- | Promoted data type which indicates if 'Peer' is used in
-- pipelined mode or not.
--
data IsPipelined = NonPipelined | Pipelined
