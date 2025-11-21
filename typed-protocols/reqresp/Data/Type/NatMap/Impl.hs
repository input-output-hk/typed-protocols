{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Data.Type.NatMap.Impl
  ( SingNatMap (..)
  , SingOrdering (..)
  -- * Lookup
  , Lookup
  , lookup
  -- ** Increment APIs
  , Increment
  , increment
  , incrementLemma
  , incrementLemma2
  -- ** Decrement APIs
  , Decrement
  , Decrement'
  , decrement
  , decrement'
  , decrementLemma
  , decrementLemma2
  -- * Extras
  , CompareWithHead
  )
  where

import Prelude hiding (head, lookup)
import Data.Type.NatMap.Types
import Data.Typeable
import Data.Void
import Network.TypedProtocol.ReqResp.Core


type        Add :: N -> N -> N
type family Add n m where
  Add Z     m = m
  Add (S n) m = S (Add n m)

--
-- Lookup with default value
--

-- | Lookup the value associated with a given state, or `Z` if missing.
--
type        Lookup :: ps -> NatMap ps -> N
type family Lookup st q where
  Lookup st Empty = Z
  Lookup st (Cons (st' :-> n') q') = Lookup' (CompareStates st st') st n' q'

type        Lookup' :: Ordering -> ps -> N -> NatMap ps -> N
type family Lookup' ord st n q where
  Lookup' 'EQ st n q = n
  Lookup' 'GT st n q = Lookup st q
  Lookup' 'LT st n q = Z


-- | Compare a state with the head of a `NatMap`, if `NatMap` is empty it
-- returns `LT`.
--
-- This is used to ensure that `SingNatMap` operations maintain order of the
-- `NatMap`.  `NatMap` is not sorted at the type level, but we ensure that all
-- operations on `SingNatMap` preserve the odder of states.
--
type        CompareWithHead :: State ps -> NatMap (State ps) -> Ordering
type family CompareWithHead st natMap where
  CompareWithHead st (Cons (st' :-> n) as) = CompareStates st st'
  CompareWithHead st Empty = LT

--
-- Increment
--

-- | We assume that the natMap is sorted and de-duplicated.
-- 
-- NOTE: 'Increment' preserves normalisation of the `NatMap`.
type        Increment :: ps -> NatMap ps -> NatMap ps
type family Increment st natMap where
  Increment st Empty = Cons (st :-> S Z) Empty
  Increment st (Cons (st' :-> n) as) = Increment' st (CompareStates st st') st' n as

type        Increment' :: ps -> Ordering -> ps -> N -> NatMap ps -> NatMap ps
type family Increment' st ord st' n natMap where
  Increment' st 'EQ st  n as = Cons (st  :-> S n) as
  Increment' st 'GT st' n as = Cons (st' :->   n) (Increment st as)
  Increment' st 'LT st' n as = Cons (st  :-> S Z) (Cons (st' :-> n) as)

--
-- Decrement
--

-- | We assume that the natMap is sorted and de-duplicated.
--
-- NOTE: 'Decrement' preserves normalisation of the `NatMap`.
type        Decrement :: ps -> NatMap ps -> NatMap ps
type family Decrement st natMap where
  Decrement st Empty = Empty
  Decrement st (Cons (st' :-> n) as) = Decrement' st (CompareStates st st') st' n as

type        Decrement' :: ps -> Ordering -> ps -> N -> NatMap ps -> NatMap ps
type family Decrement' st ord st' n natMap where
  Decrement' st 'EQ st   Z    as = as
  Decrement' st 'EQ st  (S Z) as = as
  Decrement' st 'EQ st  (S (S n)) as = Cons (st  :-> S n) as
  Decrement' st 'GT st'    n  as = Cons (st' :-> n) (Decrement st as)
  Decrement' st 'LT st'    n  as = Cons (st' :-> n) as

--
-- Singletons, term level operations
--


type SingNatMap :: NatMap (State ps) -> Type
data SingNatMap q where
  SingCons  :: forall ps (st :: State ps) (n :: N) (q :: NatMap (State ps)).
               Typeable st
            => CompareWithHead st q :~: LT
            -> SingState st
            -> Nat (S n)
            -> SingNatMap q
            -> SingNatMap (Cons (st :-> S n) q)
  SingEmpty :: SingNatMap Empty


lookup
  :: forall ps (st :: State ps) (natMap :: NatMap (State ps)).
     Compare ps
  => SingState st
  -> SingNatMap natMap
  -> Nat (Lookup st natMap)
lookup _   SingEmpty = Zero
lookup st (SingCons _ st' n as) =
  case st `compareStates` st' of
    SingEQ -> n
    SingGT -> lookup st as
    SingLT -> Zero


--
-- `increment` and its friends
--

increment :: forall ps (st :: State ps) (natMap :: NatMap (State ps)).
             Compare ps
          => Typeable st
          => SingState st
          -> SingNatMap natMap
          -> SingNatMap (Increment st natMap)
increment st  SingEmpty = SingCons Refl st (Succ Zero) SingEmpty
increment st (SingCons r'@Refl st' n' as@SingEmpty) =
  case ( st `compareStates` st'
       , symmetryS st st'
       ) of
    (SingEQ, _) -> SingCons r' st' (Succ n') as
    (SingGT, Refl) -> SingCons r' st' n' (increment st as)
    (SingLT, _)  ->
      SingCons Refl st (Succ Zero)
        (SingCons r' st' n' as)
increment st (SingCons r'@Refl st' n' as@(SingCons Refl st'' _ _)) =
    case ( st `compareStates` st'
         , symmetryS st st'
         ) of
      (SingEQ, _) -> SingCons r' st' (Succ n') as
      (SingGT, Refl) ->
        case st `compareStates` st'' of
          SingEQ -> SingCons r' st' n' (increment st as)
          SingLT -> SingCons r' st' n' (increment st as)
          SingGT -> SingCons r' st' n' (increment st as)
      (SingLT, _)  ->
        SingCons Refl st (Succ Zero)
          (SingCons r' st' n' as)


incrementLemma
  :: forall ps (st :: State ps) (st' :: State ps) (q :: NatMap (State ps)).
     Compare ps
  => (st :~: st' -> Void)
  -> SingState st
  -> SingState st'
  -> SingNatMap q
  -> Lookup st' (Increment st q)
     :~:
     Lookup st' q
incrementLemma neq st st' SingEmpty =
  case ( st `compareStates` st'
       , symmetryS st st'
       ) of
    (SingLT, Refl) -> Refl
    (SingEQ, _)    -> absurd (neq Refl)
    (SingGT, Refl) -> Refl

incrementLemma neq st st' (SingCons _ (st'' :: SingState st'') _ SingEmpty) =
  case ( st `compareStates` st'
       , symmetryS st st'
       ) of
    (SingLT, Refl) ->
      case st `compareStates` st'' of
        SingLT -> Refl
        SingEQ -> Refl
        SingGT ->
          case st' `compareStates` st'' of
            SingLT -> Refl
            SingGT -> Refl
    (SingEQ, _) -> absurd (neq Refl)
    (SingGT, Refl) ->
      case  st `compareStates` st'' of
        SingLT ->
          case st' `compareStates` st'' of
            SingLT -> Refl
            SingGT -> Refl
        SingEQ -> Refl
        SingGT ->
          case st' `compareStates` st'' of
            SingLT -> Refl
            SingEQ -> Refl
            SingGT -> Refl

incrementLemma neq st st' (SingCons Refl (st'' :: SingState st'') _ q'''@(SingCons _ (st''' :: SingState st''') _ _)) =
  case st `compareStates` st'' of
    SingLT ->
      case ( st' `compareStates` st
           , symmetryS st' st
           ) of
        -- st  < st''
        -- st' < st
        (SingLT, _)    -> case transitivityS st' st st'' of Refl -> Refl
        -- st' ≡ st
        (SingEQ, _)    -> absurd (neq Refl)
        (SingGT, Refl) -> Refl

    SingEQ ->
      case st' `compareStates` st of
        SingLT -> Refl
        SingEQ -> absurd (neq Refl)
        SingGT -> Refl

    SingGT ->
      case st' `compareStates` st'' of
        SingLT -> Refl
        SingEQ -> Refl
        SingGT ->
          case st `compareStates` st''' of
            -- st'  > st''
            -- st'' < st'''
            -- st   < st'''
            SingLT ->
              case st' `compareStates` st of
                -- st' < st
                -- by transitivity
                -- st' < st'''
                SingLT -> case transitivityS st' st st''' of Refl -> Refl
                SingEQ -> absurd (neq Refl)
                SingGT -> Refl
            SingEQ ->
              case st' `compareStates` st of
                SingLT -> Refl
                SingEQ -> absurd (neq Refl)
                SingGT -> Refl
            SingGT ->
              case st' `compareStates` st''' of
                SingLT -> Refl
                SingEQ -> Refl
                SingGT -> incrementLemma neq st st' q'''


-- | Increment a `NatMap` and provide evidence that the corresponding counter
-- increased.
--
incrementLemma2
  :: forall ps (st :: State ps) (q :: NatMap (State ps)).
     Compare ps
  => Typeable st
  => SingState st
  -> SingNatMap q
  -> Lookup st (Increment st q) :~: S (Lookup st q)
incrementLemma2 st SingEmpty =
  case ( st `compareStates` st
       , reflexivityS st
       ) of
    (SingEQ, _) -> Refl
    (_, a) -> case a of {}
incrementLemma2 st (SingCons _ st' _ q'@SingEmpty) =
  case ( st `compareStates` st'
       , symmetryS st st'
       ) of
    (SingEQ, _)    -> Refl
    (SingGT, Refl) -> incrementLemma2 st q'
    (SingLT, _)    -> 
      case ( st `compareStates` st
           , reflexivityS st
           ) of
        (SingEQ, Refl) -> Refl
        (_, a) -> case a of {}
incrementLemma2 st (SingCons Refl st' _ q'@SingCons{}) =
  case ( st `compareStates` st'
       , symmetryS st st'
       ) of
    (SingEQ, Refl) -> Refl
    (SingGT, Refl) -> incrementLemma2 st q'
    (SingLT, Refl) -> 
      case ( st `compareStates` st
           , reflexivityS st
           ) of
        (SingEQ, Refl) -> Refl
        (_, a) -> case a of {}

--
-- `decrement` and `decrement'`
--

decrement :: forall ps (st :: State ps) (q :: NatMap (State ps)) (n :: N).
             Compare ps
          => Lookup st q ~ S n
          => SingState st
          -> SingNatMap q
          -> ( SingNatMap (Decrement st q)
             , Nat (Lookup st q)
             )
decrement st (SingCons _ st' n' q'@SingEmpty) =
  case st `compareStates` st' of
    SingEQ ->
      case n' of
        Succ Zero      -> (q', n')
        Succ n''@Succ{} -> (SingCons Refl st' n'' q', n')
decrement st (SingCons Refl st' n' q'@(SingCons Refl st'' _ _)) =
  case ( st `compareStates` st'
       , symmetryS st st'
       ) of
    (SingEQ, Refl) ->
      case n' of
        Succ Zero      -> (q', n')
        Succ n''@Succ{} -> (SingCons Refl st' n'' q', n')
    (SingGT, Refl) ->
      case decrement st q' of
        -- q'' is a decremented q'
        (q'', n''@(Succ Succ{})) ->
          case st `compareStates` st'' of
            SingEQ -> (SingCons Refl st' n' q'', n'')
            SingGT -> (SingCons Refl st' n' q'', n'')
        (q''@SingEmpty, n''@(Succ Zero)) ->
          (SingCons Refl st' n' q'', n'')
        (q''@(SingCons Refl st''' _ _), n''@(Succ Zero)) ->
          case ( st `compareStates` st''
               , st' `compareStates` st'''
               ) of
            (SingEQ, SingLT) -> (SingCons Refl st' n' q'', n'')
            (SingEQ, SingGT) ->
              -- the second component states that
              -- st' > st'''
              -- which is contradiction with:
              -- st' < st'' < st'''
              case transitivityS st' st'' st''' of {}
            (SingGT, _) -> (SingCons Refl st' n' q'', n'')


-- | Like `decrement` but without the `Lookup st q ≡ S n` constraint.
--
decrement' :: forall ps (st :: State ps) (natMap :: NatMap (State ps)) (n :: N).
             Compare ps
          => Lookup st natMap ~ n
          => SingState st
          -> SingNatMap natMap
          -> (SingNatMap (Decrement st natMap), Nat n)
decrement' _st SingEmpty = (SingEmpty, Zero)
decrement' st q@(SingCons _ st' n q'@SingEmpty) =
  case st `compareStates` st' of
    SingEQ ->
      case n of
        Succ Zero      -> (q', n)
        Succ n'@Succ{} -> (SingCons Refl st' n' q', n)
    SingLT -> (q, Zero)
    SingGT -> (q, Zero)
decrement' st q@(SingCons Refl st' n q'@(SingCons Refl st'' _ _)) =
  case ( st `compareStates` st'
       , symmetryS st st'
       ) of
    (SingLT, Refl) -> (q, Zero)
    (SingEQ, Refl) ->
      case n of
        Succ Zero      -> (q', n)
        Succ n'@Succ{} -> (SingCons Refl st' n' q', n)

    -- st' < st
    (SingGT, Refl) ->
      case decrement' st q' of
        -- q'' is a decremented q'
        (q'', n'@(Succ Succ{})) ->
          case st `compareStates` st'' of
            SingEQ -> (SingCons Refl st' n q'', n')
            SingGT -> (SingCons Refl st' n q'', n')

        (q''@SingEmpty, n'@Zero) -> (SingCons Refl st' n q'', n')
        (q''@SingEmpty, n'@(Succ Zero)) ->
          (SingCons Refl st' n q'', n')

        (q''@(SingCons Refl st''' _ _), n'@Zero) ->
          case st'' `compareStates` st''' of
            SingEQ -> (SingCons Refl st' n q'', n')
            SingLT ->
              case transitivityS st' st'' st''' of
                Refl -> (SingCons Refl st' n q'', n')
            SingGT ->
              -- it is impossible that `st'' > st'''`, since `st''` is the head
              -- of `q` while `st'''` is the head of `decrement st q`.
              absurd (decrementLemmaHead st q' Refl)
        (q''@(SingCons Refl st''' _ _), n'@(Succ Zero)) ->
          case ( st `compareStates` st''
               , st' `compareStates` st'''
               ) of
            (SingEQ, SingLT) -> (SingCons Refl st' n q'', n')
            (SingEQ, SingGT) ->
              -- the second component states that
              -- st' > st'''
              -- which is contradiction with:
              -- st' < st'' < st'''
              case transitivityS st' st'' st''' of {}
            (SingGT, _) -> (SingCons Refl st' n q'', n')


--
-- Lemmas for `decrement` and `decrement'` APIs
--

-- | A lemma which proves that decrementing does not increase the head of the
-- queue, e.g. head of `Decrement st q` is not greater than head of `q`.
decrementLemmaHead :: forall ps (st :: State ps) (st' :: State ps) (q' :: NatMap (State ps)) (n' :: N).
             Compare ps
          => SingState st
          -> SingNatMap (Cons (st' :-> S n') q')
          -> ( CompareWithHead st' (Decrement st (Cons (st' :-> S n') q')) :~: GT
               -> Void)
decrementLemmaHead st (SingCons Refl st' n' _) r =
  case ( st `compareStates` st'
       , n'
       ) of
    (SingEQ, Succ (Succ _)) ->
      case r of {}
    (SingEQ, Succ Zero) ->
      case r of {}

    (SingLT, _) ->
      case reflexivityS st' of
        Refl -> case r of {}

    (SingGT, _) ->
      case reflexivityS st' of
        Refl -> case r of {}


-- | A proof that decrementing a state counter does not affect other states.
--
-- st ≠ st' ⇒ Lookup st' (Decrement st q) ≡ Lookup st' q
decrementLemma :: forall ps (st :: State ps) (st' :: State ps) (q :: NatMap (State ps)).
      Compare ps
   => (st :~: st' -> Void)
   -> SingState st
   -> SingState st'
   -> SingNatMap q
   -> Lookup st' (Decrement st q)
      :~:
      Lookup st' q

decrementLemma _ _ _  SingEmpty = Refl

decrementLemma neq st st' (SingCons _ (st'' :: SingState st'') n'' q''@SingEmpty) =
  case st `compareStates` st'' of
    SingLT -> Refl
    SingEQ ->
      case st' `compareStates` st'' of
        -- st' < st = st''
        SingLT -> case decrementLemma' neq st st' n'' q'' of
          Refl -> Refl

        -- st = st' = st''
        SingEQ -> absurd (neq Refl)

        -- st = st'' < st'
        SingGT -> decrementLemma' neq st st' n'' q''
    SingGT -> Refl

decrementLemma neq st st' (SingCons Refl (st'' :: SingState st'') n'' q''@(SingCons _ st''' _ _)) =
  case st `compareStates` st'' of
    SingLT -> Refl
    SingEQ ->
      case st' `compareStates` st'' of
        -- st = st'' & st' < st''
        SingLT ->
          case decrementLemma' neq st st' n'' q'' of
            Refl -> case st' `compareStates` st''' of
              SingLT -> Refl

              --   st' < st''
              --   st' = st'''
              -- ⇒ st''' < st''
              -- which contradicts `st'' < st'''`
              SingEQ -> case symmetryS st'' st''' of {}

              -- st'  < st''
              -- st'' < st'''
              -- which contradicts `st' > st'''`
              SingGT -> case transitivityS st' st'' st''' of {}

        -- st = st' = st''
        SingEQ -> absurd (neq Refl)

        -- st = st'' < st'
        SingGT -> decrementLemma' neq st st' n'' q''
    SingGT ->
      case st' `compareStates` st'' of
        SingLT -> Refl
        SingEQ -> Refl
        -- st > st'' & st' > st''
        SingGT -> case decrementLemma neq st st' q'' of
          Refl -> Refl


-- | A helper for `decrementLemma` which walks through `Decrement'` cases.
--
decrementLemma'
  :: forall ps (st :: State ps) (st' :: State ps) (n :: N) (q :: NatMap (State ps)).
     Compare ps
  => CompareWithHead st q ~ LT
  => (st :~: st' -> Void)
  -> SingState st
  -> SingState st'
  -> Nat n
  -> SingNatMap q
  -> Lookup st' (Decrement' st EQ st n q)
     :~:
     Lookup st' q

decrementLemma' _ _ _ Zero _ = Refl
decrementLemma' _ _ _ (Succ Zero) _ = Refl
decrementLemma' neq st st' (Succ (Succ _)) SingEmpty =
    case st' `compareStates` st of
      -- st' < st
      SingLT -> Refl
      SingEQ -> absurd (neq Refl)
      SingGT -> Refl

decrementLemma' neq st st' (Succ (Succ _)) (SingCons _ (st'' :: SingState st'') (Succ _) _) =
    case st' `compareStates` st of
      -- st' < st
      SingLT ->
        case st' `compareStates` st'' of
          -- st' < st, st' < st''
          SingLT -> Refl

          -- st' = st'' < st
          -- but we know that `st < st''` since `st''` is after `st` in
          -- the `NatMap`.
          SingEQ -> error "NatMap: invariant violation"

          -- st'' < st' < st
          -- but we know that `st < st''` since `st''` is after `st` in
          -- the `NatMap`.
          SingGT -> case symmetryS st' st'' of
            Refl ->
              case transitivityS st'' st' st of
                -- st'' < st & st < st''
                Refl ->
                  -- st'' < st & st'' > st
                  case symmetryS st'' st of {}
      -- st' = st
      SingEQ -> absurd (neq Refl)

      SingGT -> Refl
    

-- | A proof that decrementing a state counter does decrease the lookup, e.g.
--
-- Lookup st q ≡ S n ⇒ Lookup st (Decrement st q) ≡ n
--
-- 
decrementLemma2 :: forall ps (st :: State ps) (n :: N) (q :: NatMap (State ps)).
      ( Compare ps
      , Lookup st q ~ S n
      )
   => SingState st
   -> SingNatMap q
   -> Lookup st (Decrement st q) :~: n
decrementLemma2 st (SingCons Refl st' n' q') =
  case st `compareStates` st' of
    SingEQ ->
      case n' of
        Succ Zero ->
          case q' of
            SingEmpty   -> Refl
            SingCons {} -> Refl
        Succ (Succ _) -> Refl
    SingGT -> decrementLemma2 st q'
