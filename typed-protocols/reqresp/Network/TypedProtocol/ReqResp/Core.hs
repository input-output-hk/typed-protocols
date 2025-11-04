{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeData             #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}


{-# OPTIONS_HADDOCK show-extensions #-}

-- | This module defines the core of the typed protocol framework.
--
module Network.TypedProtocol.ReqResp.Core
  ( -- * Introduction
    -- $intro

    -- * Defining request-response protocols
    -- $defining
    RequestResponseProtocol (..)
  , State (..)
  , SingState (..)
  , ClientState (..)
  , SingClientState (..)
  , RequestTypeI (..)
  , SingOrdering (..)
  , Compare (..)
  , CompareStates
  , compareStates
  , reflexivityS
  , transitivityS
  , symmetryS
  , PeerRole (..)
    -- * Engaging in protocols
    -- * N and Nat
  , N (..)
  , Nat (Succ, Zero)
  , natToInt
  , unsafeIntToNat
    -- * Re-exports
  , type Type
  , (:~:)(Refl)
  ) where

import Data.Kind (Type)
import Data.Typeable ((:~:)(Refl))
import Data.Type.NatMap.Types
import Data.Singletons

import Network.TypedProtocol.Core (N (..), Nat (Succ, Zero), natToInt, unsafeIntToNat)

-- | A class for accessing the singleton request type for a given state.  This
-- instance is analogous to `SingI` from the `singletons` library.
--
class RequestTypeI st where
  requestType :: RequestType st

-- | The library provides the only `Terminal` request type.  All user-provided
-- request types are `Active`.  For `Active` state the server can continue
-- listening for incoming requests, in `Terminal` state, the server can only
-- respond for outstanding request or send termination response.
--
type data State ps = Active ps | Terminal

type SingState :: State ps -> Type
data SingState st where
  SingActive   :: forall ps (st :: ps).
                  RequestType st
               -> SingState (Active st)
  SingTerminal :: SingState Terminal

deriving instance Show (RequestType st) => Show (SingState (Active st))
deriving instance Show (SingState Terminal)

type SingOrdering :: Ordering -> ps -> ps -> Type
data SingOrdering ord st st' where
  SingLT :: SingOrdering 'LT st st'
  SingEQ :: SingOrdering 'EQ st st
  SingGT :: SingOrdering 'GT st st'
-- note: if `Sing{LT,GT}` carried `st :~: st' -> Void` then we could eliminate
-- some cases in `Proofs` (see `illegal Compare instance`), but is it worth it
-- to ask the user to provide such proofs.

type data ClientState = Running | Terminated
type SingClientState :: ClientState -> Type
data SingClientState ps where
  SingRunning    :: SingClientState Running
  SingTerminated :: SingClientState Terminated

type instance Sing = SingClientState
instance SingI Running where sing = SingRunning
instance SingI Terminated where sing = SingTerminated

type        Symmetry :: Ordering -> Ordering
type family Symmetry o where
  Symmetry LT = GT
  Symmetry EQ = EQ
  Symmetry GT = LT

class Compare ps where
  -- | A type family that compares types of kind `ps`.
  type CompareTypes (t :: ps) (t' :: ps) :: Ordering

  -- | A term level function that compares singletons of type `ps`, compatible
  -- with `CompareTypes` type family.
  compareTypes
    :: forall (t :: ps) (t' :: ps).
       RequestType t
    -> RequestType t'
    -> SingOrdering (CompareTypes t t') t t'


  reflexivity  :: forall (t :: ps).
                  RequestType t
               -> CompareTypes t t :~: EQ

  transitivity :: forall (t :: ps) (t' :: ps) (t'' :: ps).
                  ( CompareTypes t  t' ~ LT
                  , CompareTypes t' t'' ~ LT
                  )
               => RequestType t
               -> RequestType t'
               -> RequestType t''
               -> CompareTypes t t'' :~: LT

  symmetry     :: forall (t :: ps) (t' :: ps).
                  RequestType t
               -> RequestType t'
               -> CompareTypes t' t :~: Symmetry (CompareTypes t t')


-- | An extension of `CompareTypes` type family to `State ps`.
type        CompareStates :: State ps -> State ps -> Ordering
type family CompareStates st st' where
  CompareStates (Active st) (Active st') = CompareTypes st st'
  CompareStates (Active _)   Terminal    = LT
  CompareStates Terminal     (Active _)  = GT
  CompareStates Terminal     Terminal    = EQ


-- | An extension of `compareTypes` to `State ps`.
--
compareStates
  :: forall ps (st :: State ps) (st' :: State ps).
     Compare ps
  => SingState st
  -> SingState st'
  -> SingOrdering (CompareStates st st') st st'
compareStates (SingActive st) (SingActive st') =
  case st `compareTypes` st' of
    SingEQ -> SingEQ
    SingLT -> SingLT
    SingGT -> SingGT
compareStates (SingActive _) SingTerminal = SingLT
compareStates SingTerminal (SingActive _) = SingGT
compareStates SingTerminal SingTerminal   = SingEQ


reflexivityS :: forall ps (st :: State ps).
                 Compare ps
              => SingState st
              -> CompareStates st st :~: EQ
reflexivityS (SingActive st) = reflexivity @ps st
reflexivityS SingTerminal    = Refl


transitivityS
  :: forall ps (st :: State ps)
               (st' :: State ps)
               (st'' :: State ps).
     ( Compare ps
     , CompareStates st  st'  ~ LT
     , CompareStates st' st'' ~ LT
     )
  => SingState st
  -> SingState st'
  -> SingState st''
  -> CompareStates st st'' :~: LT
transitivityS (SingActive st) (SingActive st') (SingActive st'') =
  transitivity st st' st''
transitivityS (SingActive _) _ SingTerminal = Refl
transitivityS SingTerminal a _ = case a of {}


symmetryS :: forall ps (st :: State ps) (st' :: State ps).
             Compare ps
          => SingState st
          -> SingState st'
          -> CompareStates st' st :~: Symmetry (CompareStates st st')
symmetryS (SingActive st) (SingActive st') = symmetry @ps st st'
symmetryS SingActive{} SingTerminal = Refl
symmetryS SingTerminal SingActive{} = Refl
symmetryS SingTerminal SingTerminal = Refl


type data PeerRole ps = AsClient ClientState
                      | AsServer (NatMap (State ps))


-- | A Request-response protocol.
--
-- Constraints:
--
-- * there's a unique termination request `Request ps Terminate`;
-- * there's no server side response to a termination request.
--
-- Termination request locks the server side, it can only respond to
-- outstanding requests.
--
--
class Compare ps => RequestResponseProtocol ps where

  -- | The data family for requests.  It is indexed by `State ps` which allows
  -- to define a termination request.
  data Request  ps (st :: State ps)

  -- | The data family for responses.  It is indexed by `ps`, so that the
  -- termination request doesn't require a response.
  --
  data Response ps (st :: ps)

  -- | A type family that associates each protocol with a singleton data type.
  -- This is analogous to `Sing` from the `singletons` library.
  --
  type RequestType :: ps -> Type
