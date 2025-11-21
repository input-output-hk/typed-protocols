{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}


-- This is already implied by the -Wall in the .cabal file, but lets just be
-- completely explicit about it too, since we rely on the completeness
-- checking in the cases below for the completeness of our proofs.
{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- redundant-constraints is needed in `decrementLemmaCbs'` where
-- `CompareWithHead` is used to eliminate some impossible cases, but GHC is not
-- clever enough to figure it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Proofs about the typed protocol framework.
--
-- It also provides helpful testing utilities.
--
module Network.TypedProtocol.ReqResp.Proofs
  ( connect
  ) where

import Prelude hiding (length)
import Control.Monad.Class.MonadSay
import Data.Type.NatMap

import Network.TypedProtocol.ReqResp.Core
import Network.TypedProtocol.ReqResp.Peer


-- | A vector of `n :: N` values.
type Vec :: N -> Type -> Type
data Vec n a where
    VecCons  :: a -> Vec n a -> Vec (S n) a
    VecEmpty :: Vec Z a

length :: Vec n a -> Nat n
length VecEmpty = Zero
length (VecCons _ as) = Succ (length as)

-- | Callback for a given request type.
data Cb ps (st :: State ps) m where
  CbActive   :: (Response ps st -> m ())
             -> Cb ps (Active st) m
  CbTerminal :: Cb ps Terminal m


-- | A singleton representation of a `NatMap` which stores callbacks registered
-- by the client preserving the order of request types.
--
type SingNatMapCbs :: NatMap (State ps)
                   -> (Type -> Type)
                   -> Type
data SingNatMapCbs q m where
  SingConsCbs
    :: forall ps (st :: State ps) (n :: N) (q :: NatMap (State ps)) m.
       CompareWithHead st q :~: LT
    -> SingState st
    -> Vec (S n) (Cb ps st m)
    -> SingNatMapCbs q m
    -> SingNatMapCbs (Cons (st :-> S n) q) m
  SingEmptyCbs
    :: SingNatMapCbs Empty m


incrementCbs
  :: forall ps (st :: State ps) (natMap :: NatMap (State ps)) m.
     Compare ps
  => SingState st
  -> Cb ps st m
  -> SingNatMapCbs natMap m
  -> SingNatMapCbs (Increment st natMap) m
incrementCbs st handler SingEmptyCbs =
  SingConsCbs Refl st (VecCons handler VecEmpty) SingEmptyCbs
incrementCbs st handler (SingConsCbs r'@Refl st' vec' as@SingEmptyCbs) =
  case ( st `compareStates` st'
       , symmetryS st st'
       ) of
    (SingEQ, _)    -> SingConsCbs r' st' (VecCons handler vec') as
    (SingGT, Refl) -> SingConsCbs r' st' vec' (incrementCbs st handler as)
    (SingLT, _)    -> SingConsCbs Refl st (VecCons handler VecEmpty)
                        (SingConsCbs r' st' vec' as)
incrementCbs st handler (SingConsCbs Refl st' vec as@(SingConsCbs Refl st'' _ _)) =
  case ( st `compareStates` st'
       , symmetryS st st'
       ) of
    (SingEQ, _) -> SingConsCbs Refl st' (VecCons handler vec) as
    (SingGT, Refl) ->
      case st `compareStates` st'' of
        SingEQ -> SingConsCbs Refl st' vec
                    (incrementCbs st handler as)
        SingLT -> SingConsCbs Refl st' vec
                    (incrementCbs st handler as)
        SingGT -> SingConsCbs Refl st' vec
                    (incrementCbs st handler as)
    (SingLT, _) ->
      SingConsCbs Refl st (VecCons handler VecEmpty)
        (SingConsCbs Refl st' vec as)


incrementLemmaCbs
  :: forall ps (st :: ps) (q :: NatMap (State ps)) m.
     Compare ps
  => SingState (Active st)
  -> SingNatMapCbs q m
  -> Lookup Terminal (Increment (Active st) q)
     :~:
     Lookup Terminal q
incrementLemmaCbs _ SingEmptyCbs = Refl
incrementLemmaCbs st (SingConsCbs _ st' _ q') =
  case st `compareStates` st' of
    SingLT -> Refl
    SingEQ -> Refl
    SingGT ->
      case SingTerminal `compareStates` st' of
        SingLT ->
          case incrementLemmaCbs st q' of
            Refl -> Refl
        SingGT ->
          case incrementLemmaCbs st q' of
            Refl -> Refl


incrementLemma2Cbs
  :: forall ps (st :: State ps) (q :: NatMap (State ps)) m.
     Compare ps
  => SingState st
  -> SingNatMapCbs q m
  -> Lookup st (Increment st q) :~: S (Lookup st q)
incrementLemma2Cbs st SingEmptyCbs =
  case ( st `compareStates` st
       , reflexivityS st
       ) of
    (SingEQ, Refl) -> Refl
    (_, a) -> case a of {}
incrementLemma2Cbs st (SingConsCbs _ st' _ q'@SingEmptyCbs) =
  case st `compareStates` st' of
    SingEQ -> Refl
    SingGT -> incrementLemma2Cbs st q'
    SingLT -> 
      case ( st `compareStates` st
           , reflexivityS st
           ) of
        (SingEQ, _) -> Refl
        (_,      a) -> case a of {}
incrementLemma2Cbs st (SingConsCbs _ st' _ q') =
  case st `compareStates` st' of
    SingEQ -> Refl
    SingGT -> incrementLemma2Cbs st q'
    SingLT -> 
      case ( st `compareStates` st
           , reflexivityS st
           ) of
        (SingEQ, _) -> Refl
        (_, a) -> case a of {}


decrementCbs
  :: forall ps (st :: State ps) (q :: NatMap (State ps)) (n :: N) m.
     Compare ps
  => Lookup st q ~ S n
  => SingState st
  -> SingNatMapCbs q m
  -> ( SingNatMapCbs (Decrement st q) m
     , Cb ps st m
     , Nat (Lookup st q)
     )
decrementCbs st (SingConsCbs _ st' vec' q'@SingEmptyCbs) =
  case st `compareStates` st' of
    SingEQ ->
      case vec' of
        VecCons cb VecEmpty        -> (q', cb, length vec')
        VecCons cb vec''@VecCons{} -> (SingConsCbs Refl st' vec'' q', cb, length vec')
decrementCbs st (SingConsCbs Refl st' vec' q'@(SingConsCbs Refl st'' _ _)) =
  case ( st `compareStates` st'
       , symmetryS st st'
       )
       of
    (SingEQ, Refl) ->
      case vec' of
        VecCons cb VecEmpty        -> (q', cb, length vec')
        VecCons cb vec''@VecCons{} -> (SingConsCbs Refl st' vec'' q', cb, length vec')
    (SingGT, Refl) ->
      case decrementCbs st q' of
        (q'', cb, n''@(Succ Succ{})) ->
          case st `compareStates` st'' of
            SingEQ -> (SingConsCbs Refl st' vec' q'', cb, n'')
            SingGT -> (SingConsCbs Refl st' vec' q'', cb, n'')
        (q''@SingEmptyCbs, cb, n''@(Succ Zero)) ->
          (SingConsCbs Refl st' vec' q'', cb, n'')
        (q''@(SingConsCbs Refl st''' _ _), cb, n''@(Succ Zero)) ->
          case ( st `compareStates` st''
               , st' `compareStates` st'''
               ) of
            (SingEQ, SingLT) -> (SingConsCbs Refl st' vec' q'', cb, n'')
            (SingEQ, SingGT) ->
              -- the second component states that
              -- st' > st'''
              -- which is contradiction with:
              -- st' < st'' < st'''
              case transitivityS st' st'' st''' of {}
            (SingGT, _) -> (SingConsCbs Refl st' vec' q'', cb, n'')


decrementLemmaCbs
  :: forall ps (st :: ps) (q :: NatMap (State ps)) m.
     Compare ps
  => SingState (Active st)
  -> SingNatMapCbs q m
  -> Lookup Terminal (Decrement (Active st) q)
     :~:
     Lookup Terminal q
decrementLemmaCbs _    SingEmptyCbs = Refl

decrementLemmaCbs st (SingConsCbs _ (st'' :: SingState st'') vec'' q''@SingEmptyCbs) =
  case st `compareStates` st'' of
    SingLT -> Refl
    SingEQ ->
      case SingTerminal `compareStates` st'' of
        SingGT -> decrementLemmaCbs' st vec'' q''
    SingGT -> Refl

decrementLemmaCbs st (SingConsCbs Refl (st'' :: SingState st'') vec'' q''@SingConsCbs{}) =
  case st `compareStates` st'' of
    SingLT -> Refl
    SingEQ ->
      case SingTerminal `compareStates` st'' of
        -- Active st = st'' < st'
        SingGT -> decrementLemmaCbs' st vec'' q''
    SingGT ->
      case SingTerminal `compareStates` st'' of
        SingLT -> Refl
        -- st > st'' & st' > st''
        SingGT -> decrementLemmaCbs st q''


decrementLemmaCbs'
  :: forall ps (st :: ps) (st' :: State ps) (q :: NatMap (State ps)) (n :: N) m.
     Compare ps
  => CompareWithHead (Active st) q ~ LT
  => SingState (Active st)
  -> Vec n (Cb ps st' m)
  -> SingNatMapCbs q m
  -> Lookup Terminal (Decrement' (Active st) EQ (Active st) n q)
     :~:
     Lookup Terminal q
decrementLemmaCbs' _            VecEmpty   _            = Refl
decrementLemmaCbs' _ (VecCons _ VecEmpty)  _            = Refl
decrementLemmaCbs' _ (VecCons _ VecCons{}) SingEmptyCbs = Refl

decrementLemmaCbs' st (VecCons _ VecCons{}) (SingConsCbs _ (st'' :: SingState st'') VecCons{} _) =
    case st `compareStates` SingTerminal of
      -- st' < st
      SingLT ->
        case st `compareStates` st'' of
          -- st' < st, st' < st''
          SingLT -> Refl


-- | This type is a wrapper around `SingNatMapCbs` which binds the client state
-- `Running` with server's `Lookup Terminal q ~ Z` constraint.
type ClientServerState :: forall ps -> ClientState -> NatMap (State ps) -> (Type -> Type) -> Type
data ClientServerState ps cs q m where
  -- client hasn't yet send the terminal request
  ClientActive
    :: forall ps (q :: NatMap (State ps)) m.
       Lookup Terminal q ~ Z
    => SingNatMapCbs q m
    -> ClientServerState ps Running q m


-- | Run a client and server until both terminate.
--
-- This proves that client and the server will run until their completion, in
-- particular that:
--
-- * no protocol deadlock is possible;
-- * no protocol live-lock is possible;
--
-- This proof has the nice property that the client and the server are reaching
-- `ClientDone` & `ServerDone` at the same time (unlike some previous attempts
-- for a slightly different `Peer`).  However, this is not a property of
-- evolution of the protocol.  For that we'd need to provide a synchronisation
-- mechanism, e.g. some form of `Collect` combinator from `typed-protocols`.
--
connect :: forall ps m a b.
           MonadSay m
        => Compare ps
        => Peer ps (AsClient Running) m a
        -> Peer ps (AsServer Empty) m b
        -> m (a, b)

connect = stage1 (ClientActive SingEmptyCbs)
  where
    -- The strategy for the proof.
    --
    -- There are two stages:
    --
    -- 1. The client hasn't yet sent the termination request.
    -- 2. The client has sent the termination request, the server acknowledge it.
    --
    -- In the first stage the `ClientServerState` is used to track both the
    -- client & server state binding.  In the first stage the client state is
    -- `Running` while the server state is characterised by
    -- `Lookup Terminal q ~ Z`: the `ClientServerState` does that for us.
    --
    -- After `SendRequestDone`, the server queue is characterised by `Lookup
    -- Terminal q ~ S Z`.  The server still can issue `SendResponse`
    -- for non-terminating messages or terminate with `ServerDone` but it
    -- cannot `AwaitRequest`.

    --
    -- First Stage
    --
    stage1
      :: forall (q :: NatMap (State ps)).
         ClientServerState ps Running q m
      -> Peer ps (AsClient Running) m a
      -> Peer ps (AsServer q) m b
      -> m (a, b)

    -- handle effects
    stage1 q (Effect k) server    = k >>= flip (stage1 q) server
    stage1 q client    (Effect k) = k >>=       stage1 q  client

    -- client sends a request, server accepts it
    stage1 (ClientActive q)
       (SendRequest (msg :: Request ps (Active st))
                    handler
                    (k :: Peer ps (AsClient cs) m a))
       (AwaitRequest k')
       =
       let st = SingActive (requestType @st) in
       case ( incrementCbs      st (CbActive handler) q
            , incrementLemmaCbs st q
            ) of
         (q', Refl) ->
           stage1 (ClientActive q') k (k' msg)

    -- server is sending responses to requests the client has already sent,
    -- e.g. the callback to handle the response is available in the
    -- `SingNatMapCbs`.
    stage1 (ClientActive q) client (SendResponse (msg :: Response ps st) server) =
      let st = SingActive (requestType @st) in
      case ( decrementCbs      st q
           , decrementLemmaCbs st q
           ) of
        ((q', CbActive handler, _), Refl) -> do
          handler msg
          stage1 (ClientActive q') client server

    -- client is sending its termination signal
    stage1 (ClientActive q)
           (SendRequestDone (msg :: Request ps Terminal)
                            (k :: Peer ps (AsClient cs) m a))
           (AwaitRequest k') =
      let st = SingTerminal in
      case ( incrementCbs       st CbTerminal q 
           , incrementLemma2Cbs st q
           ) of
        (q', Refl) -> stage2 q' k (k' msg)


    --
    -- Second stage
    --

    -- The client sends `Terminal` message, but the server hasn't yet received it.
    stage2
      :: forall (q :: NatMap (State ps)).
         Lookup Terminal q ~ S Z
      => SingNatMapCbs q m
      -> Peer ps (AsClient Terminated) m a
      -> Peer ps (AsServer q) m b
      -> m (a, b)

    -- server sends a response to one of outstanding requests
    -- NOTE: client is not advancing here, this explains the last comment in
    -- the haddocks for `connect`.
    stage2 q client (SendResponse (msg :: Response ps st) k) =
      let st = SingActive (requestType @st) in
      case ( decrementCbs      st q
           , decrementLemmaCbs st q
           ) of
        ((q', CbActive handler, _), Refl) -> do
          handler msg
          stage2 q' client k

    -- handle effects
    --
    -- NOTE: it is important to run client effects after `SendResponse`s.  So
    -- that all the client's callbacks are executed before the simulation
    -- terminates.   If we had `Collect` primitive, and we enforced collecting
    -- all results before termination the proof would be more robust.
    --
    stage2 q (Effect k) server    = do
      say "stage2:Effect:_"
      k >>= flip (stage2 q) server
    stage2 q client    (Effect k) = k >>=       stage2 q  client

    -- client and server are done
    stage2 _ (ClientDone a) (ServerDone b) =
      return (a, b)
