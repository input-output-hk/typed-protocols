{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


-- This is already implied by the -Wall in the .cabal file, but lets just be
-- completely explicit about it too, since we rely on the completeness
-- checking in the cases below for the completeness of our proofs.
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Proofs about the typed protocol framework.
--
-- It also provides helpful testing utilities.
--
module Network.TypedProtocol.Proofs
  ( -- * About these proofs
    -- $about
    -- * Connect proof
    connect
  , connectPipelined
  , TerminalStates (..)
    -- * Pipelining proofs
    -- | Additional proofs specific to the pipelining features
  , forgetPipelined
  , promoteToPipelined
    -- ** Pipeline proof helpers
  , Queue (..)
  , enqueue
    -- ** Auxiliary functions
  , pipelineInterleaving
  ) where

import           Data.Singletons
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Lemmas
import           Network.TypedProtocol.Peer

-- $about
--
-- Typed languages such as Haskell can embed proofs. In total languages this
-- is straightforward: a value inhabiting a type is a proof of the property
-- corresponding to the type.
--
-- In languages like Haskell that have ⊥ as a value of every type, things
-- are slightly more complicated. We have to demonstrate that the value that
-- inhabits the type of interest is not ⊥ which we can do by evaluation.
--
-- This idea crops up frequently in advanced type level programming in Haskell.
-- For example @Refl@ proofs that two types are equal have to have a runtime
-- representation that is evaluated to demonstrate it is not ⊥ before it
-- can be relied upon.
--
-- The proofs here are about the nature of typed protocols in this framework.
-- The 'connect' and 'connectPipelined' proofs rely on a few lemmas about
-- the individual protocol. See 'AgencyProofs'.




-- | The 'connect' function takes two peers that agree on a protocol and runs
-- them in lock step, until (and if) they complete.
--
-- The 'connect' function serves a few purposes.
--
-- * The fact we can define this function at at all proves some minimal
-- sanity property of the typed protocol framework.
--
-- * It demonstrates that all protocols defined in the framework can be run
-- with synchronous communication rather than requiring buffered communication.
--
-- * It is useful for testing peer implementations against each other in a
-- minimalistic setting.
--
connect
   :: forall ps (pr :: PeerRole) (initSt :: ps) m a b.
      (Monad m, SingI pr)
   => Peer ps             pr  NonPipelined initSt m a
   -> Peer ps (FlipAgency pr) NonPipelined initSt m b
   -> m (a, b, TerminalStates ps)
connect = go
  where
    singPeerRole :: Sing pr
    singPeerRole = sing

    go :: forall (st :: ps).
          Peer ps             pr  NonPipelined st m a
       -> Peer ps (FlipAgency pr) NonPipelined st m b
       -> m (a, b, TerminalStates ps)
    go (Done ReflNobodyAgency a)  (Done ReflNobodyAgency b) =
        return (a, b, terminals)
      where
        terminals :: TerminalStates ps
        terminals = TerminalStates (stateToken :: StateToken st)
                                   (stateToken :: StateToken st)

    go (Effect a )      b              = a >>= \a' -> go a' b
    go  a              (Effect b)      = b >>= \b' -> go a  b'
    go (Yield _ msg a) (Await _ b)     = go  a     (b msg)
    go (Await _ a)     (Yield _ msg b) = go (a msg) b

    -- By appealing to the proofs about agency for this protocol we can
    -- show that these other cases are impossible
    go (Yield reflA _ _) (Yield reflB _ _) =
      case exclusionLemma_ClientAndServerHaveAgency singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}

    go (Await reflA _)   (Await reflB _)   =
      case exclusionLemma_ClientAndServerHaveAgency singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}

    go (Done  reflA _) (Yield reflB _ _)   =
      case terminationLemma_2 singPeerRole reflB reflA of
        ReflNobodyHasAgency -> case reflB of {}

    go (Done  reflA _) (Await reflB _)     =
      case terminationLemma_2 singPeerRole reflB reflA of
        ReflNobodyHasAgency -> case reflB of {}

    go (Yield reflA _ _) (Done reflB _)    =
      case terminationLemma_1 singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}

    go (Await reflA _)   (Done reflB _)    =
      case terminationLemma_1 singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}


-- | The terminal states for the protocol. Used in 'connect' and
-- 'connectPipelined' to return the states in which the peers terminated.
--
data TerminalStates ps where
     TerminalStates
       :: forall ps (st :: ps).
          (StateAgency st  ~ NobodyAgency)
       => StateToken st
       -> StateToken st
       -> TerminalStates ps

--
-- Remove Pipelining
--


-- | A size indexed queue. This is useful for proofs, including
-- 'connectPipelined' but also as so-called @direct@ functions for running a
-- client and server wrapper directly against each other.
--
data Queue (n :: N) a where
  EmptyQ ::                   Queue  Z    a
  ConsQ  :: a -> Queue n a -> Queue (S n) a

-- | At an element to the end of a 'Queue'. This is not intended to be
-- efficient. It is only for proofs and tests.
--
enqueue :: a -> Queue n a -> Queue (S n) a
enqueue a  EmptyQ     = ConsQ a EmptyQ
enqueue a (ConsQ b q) = ConsQ b (enqueue a q)


-- | Prove that we have a total conversion from pipelined peers to regular
-- peers. This is a sanity property that shows that pipelining did not give
-- us extra expressiveness or to break the protocol state machine.
--
forgetPipelined
  :: forall ps (pr :: PeerRole) (st :: ps) c m a.
     Functor m
  => [Bool]
  -- ^ interleaving choices for pipelining allowed by
  -- `Collect` and `CollectSTM` primitive. False values or `[]` give no
  -- pipelining.  For the 'CollectSTM' primitive, the stm action must not
  -- block otherwise even if the choice is to pipeline more (a 'True' value),
  -- we'll actually collect a result.
  -> Peer ps pr (Pipelined Z c) st m a
  -> Peer ps pr  NonPipelined   st m a
forgetPipelined = goSender EmptyQ
  where
    goSender :: forall st' n.
                Queue n c
             -> [Bool]
             -> Peer ps pr ('Pipelined n c) st' m a
             -> Peer ps pr 'NonPipelined    st' m a

    goSender EmptyQ _cs (Done           refl     k) = Done refl k
    goSender q       cs (Effect                  k) = Effect (goSender q cs <$> k)
    goSender q       cs (Yield          refl m   k) = Yield refl m (goSender q cs k)
    goSender q       cs (Await          refl     k) = Await refl   (goSender q cs <$> k)
    goSender q       cs (YieldPipelined refl m r k) = Yield refl m (goReceiver q cs k r)
    goSender q (True:cs')       (Collect (Just k) _) = goSender q cs' k
    goSender (ConsQ x q) (_:cs) (Collect _ k)        = goSender q cs (k x)
    goSender (ConsQ x q) cs@[]  (Collect _ k)        = goSender q cs (k x)

    goReceiver :: forall stCurrent stNext n.
                  Queue n c
               -> [Bool]
               -> Peer     ps pr ('Pipelined (S n) c) stNext m a
               -> Receiver ps pr  stCurrent stNext m c
               -> Peer     ps pr 'NonPipelined stCurrent m a

    goReceiver q cs s (ReceiverDone     x) = goSender (enqueue x q) cs s
    goReceiver q cs s (ReceiverEffect   k) = Effect   (goReceiver q cs s <$> k)
    goReceiver q cs s (ReceiverAwait refl k) = Await refl (goReceiver q cs s . k)


-- | Promote a peer to a pipelined one.
--
-- This is a right inverse of `forgetPipelined`, e.g.
--
-- >>> forgetPipelined . promoteToPipelined = id
--
-- This function is useful to test a pipelined peer against a non-pipelined one
-- using `connectPipelined` function.
--
promoteToPipelined
  :: forall ps (pr :: PeerRole) st c m a.
     Functor m
  => Peer ps pr 'NonPipelined    st m a
  -> Peer ps pr ('Pipelined Z c) st m a
promoteToPipelined (Effect k)         = Effect
                                      $ promoteToPipelined <$> k
promoteToPipelined (Yield refl msg k) = Yield refl msg
                                      $ promoteToPipelined k
promoteToPipelined (Await refl k)     = Await refl
                                      $ promoteToPipelined . k
promoteToPipelined (Done refl k)      = Done refl k


-- | Analogous to 'connect' but also for pipelined peers.
--
-- Since pipelining allows multiple possible interleavings, we provide a
-- @[Bool]@ parameter to control the choices. Each @True@ will trigger picking
-- the first choice in the @SenderCollect@ construct (if possible), leading
-- to more results outstanding. This can also be interpreted as a greater
-- pipeline depth, or more messages in-flight.
--
-- This can be exercised using a QuickCheck style generator.
--
connectPipelined
  :: forall ps (pr :: PeerRole)
               (st :: ps) c m a b.
       (Monad m, SingI pr)
    => [Bool]
    -> Peer ps             pr  ('Pipelined Z c) st m a
    -> Peer ps (FlipAgency pr) NonPipelined     st m b
    -> m (a, b, TerminalStates ps)
connectPipelined csA a b =
    connect (forgetPipelined csA a) b

-- | A reference specification for interleaving of requests and responses
-- with pipelining, where the environment can choose whether a response is
-- available yet.
--
-- This also supports bounded choice where the maximum number of outstanding
-- in-flight responses is limited.
--
pipelineInterleaving :: Int    -- ^ Bound on outstanding responses
                     -> [Bool] -- ^ Pipelining choices
                     -> [req] -> [resp] -> [Either req resp]
pipelineInterleaving omax cs0 reqs0 resps0 =
    go 0 cs0 (zip [0 :: Int ..] reqs0)
             (zip [0 :: Int ..] resps0)
  where
    go o (c:cs) reqs@((reqNo, req) :reqs')
               resps@((respNo,resp):resps')
      | respNo == reqNo = Left  req   : go (o+1) (c:cs) reqs' resps
      | c && o < omax   = Left  req   : go (o+1)    cs  reqs' resps
      | otherwise       = Right resp  : go (o-1)    cs  reqs  resps'

    go o []     reqs@((reqNo, req) :reqs')
               resps@((respNo,resp):resps')
      | respNo == reqNo = Left  req   : go (o+1) [] reqs' resps
      | otherwise       = Right resp  : go (o-1) [] reqs  resps'

    go _ _ [] resps     = map (Right . snd) resps
    go _ _ (_:_) []     = error "pipelineInterleaving: not enough responses"
