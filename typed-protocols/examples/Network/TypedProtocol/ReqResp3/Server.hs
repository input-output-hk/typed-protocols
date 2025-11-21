{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Network.TypedProtocol.ReqResp3.Server
  ( serverFIFO
  , serverFIFO'
  , serverMax
  ) where

import Prelude hiding (lookup)
import Data.Type.NatMap
import Network.TypedProtocol.ReqResp
import Network.TypedProtocol.ReqResp3.Type


-- | A server which responds to requests in FIFO order.
--
serverFIFO
  :: forall m. Peer ReqType (AsServer Empty) m ()
serverFIFO = go SingEmpty 0 (0, 1) 'a'
  where
    go :: forall (q :: NatMap (State ReqType)).
          q ~ Empty
       => SingNatMap q
       -> Int
       -> (Int, Int)
       -> Char
       -> Peer ReqType (AsServer q) m ()
    go q@SingEmpty i (!fib, fib') c =
      case lookup SingTerminal q of
        Zero ->
          AwaitRequest $ \case
            MsgRequestInt ->
              case increment (SingActive SingReqInt) q of
                q' ->
                  case decrement (SingActive SingReqInt) q' of
                    (q'', _) ->
                      SendResponse
                        (MsgResponseInt i)
                        (go q'' (i + 1) (fib, fib') c)

            MsgRequestFib ->
              case increment (SingActive SingReqFib) q of
                q' ->
                  case decrement (SingActive SingReqFib) q' of
                    (q'', _) ->
                      SendResponse
                        (MsgResponseFib fib)
                        (go q'' i (fib', fib + fib') c)

            MsgRequestStr ->
              case increment (SingActive SingReqStr) q of
                q' ->
                  case ( decrement (SingActive SingReqStr) q'
                       ) of
                    (q'', _) ->
                      SendResponse
                        (MsgResponseStr [c])
                        (go q'' i (fib, fib') (succ c))
            MsgDone ->
              case increment SingTerminal q of
                q' -> serverDone q'

    serverDone
      :: forall (q :: NatMap (State ReqType)).
         (q ~ Cons (Terminal:->S Z) Empty)
      => SingNatMap q
      -> Peer ReqType (AsServer q) m ()
    serverDone _q = ServerDone ()


-- | Alternative implementation of a server which responds to requests in FIFO
-- order.
--
-- Instead of using `Empty :: NatMap (State ReqType)` as we did in `serverFIFO`
-- we constraint the state with all `Lookup`s, and then we need to provide
-- proofs for each state.
--
-- NOTE: since `serverFIFO` is much simpler than `serverFIFO'`, it might be
-- worth to look if it is possible to provide functions which can automatically
-- map from one style to the other, e.g. from constraint base approach to
-- a direct description and vice versa.
--
serverFIFO'
  :: forall m. Peer ReqType (AsServer Empty) m ()
serverFIFO' = go SingEmpty 0 (0, 1) 'a'
  where
    go :: forall (q :: NatMap (State ReqType)).
          ( Lookup (Active ReqInt) q ~ Z
          , Lookup (Active ReqFib) q ~ Z
          , Lookup (Active ReqStr) q ~ Z
          , Lookup Terminal        q ~ Z
          )
       => SingNatMap q
       -> Int
       -> (Int, Int)
       -> Char
       -> Peer ReqType (AsServer q) m ()
    go q i (!fib, fib') c =
      AwaitRequest $ \case
        MsgRequestInt ->
          case ( increment (SingActive SingReqInt) q
               , incrementLemma (\case{}) (SingActive SingReqInt) SingTerminal q
               , incrementLemma (\case{}) (SingActive SingReqInt) (SingActive SingReqFib) q
               , incrementLemma (\case{}) (SingActive SingReqInt) (SingActive SingReqStr) q
               , incrementLemma2 (SingActive SingReqInt) q
               ) of
            (q', Refl, Refl, Refl, Refl) ->
              case ( decrement (SingActive SingReqInt) q'
                   , decrementLemma (\case{}) (SingActive SingReqInt) SingTerminal q'
                   , decrementLemma (\case{}) (SingActive SingReqInt) (SingActive SingReqFib) q'
                   , decrementLemma (\case{}) (SingActive SingReqInt) (SingActive SingReqStr) q'
                   , decrementLemma2 (SingActive SingReqInt) q'
                   ) of
                ((q'', _), Refl, Refl, Refl, Refl) ->
                  SendResponse
                    (MsgResponseInt i)
                    (go q'' (i + 1) (fib, fib') c)

        MsgRequestFib ->
          case ( increment (SingActive SingReqFib) q
               , incrementLemma (\case{}) (SingActive SingReqFib) SingTerminal q
               , incrementLemma (\case{}) (SingActive SingReqFib) (SingActive SingReqInt) q
               , incrementLemma (\case{}) (SingActive SingReqFib) (SingActive SingReqStr) q
               , incrementLemma2 (SingActive SingReqFib) q
               ) of
            (q', Refl, Refl, Refl, Refl) ->
              case ( decrement (SingActive SingReqFib) q'
                   , decrementLemma (\case{}) (SingActive SingReqFib) SingTerminal q'
                   , decrementLemma (\case{}) (SingActive SingReqFib) (SingActive SingReqInt) q'
                   , decrementLemma (\case{}) (SingActive SingReqFib) (SingActive SingReqStr) q'
                   , decrementLemma2 (SingActive SingReqFib) q'
                   ) of
                ((q'', _), Refl, Refl, Refl, Refl) ->
                  SendResponse
                    (MsgResponseFib fib)
                    (go q'' i (fib', fib + fib') c)

        MsgRequestStr ->
          case ( increment (SingActive SingReqStr) q
                 -- Lookup Terminal (Increment (Active ReqStr) q) ≡ Lookup Terminal q
               , incrementLemma (\case{}) (SingActive SingReqStr) SingTerminal q
               , incrementLemma (\case{}) (SingActive SingReqStr) (SingActive SingReqInt) q
               , incrementLemma (\case{}) (SingActive SingReqStr) (SingActive SingReqFib) q
               , incrementLemma2 (SingActive SingReqStr) q
               ) of
            (q', Refl, Refl, Refl, Refl) ->
              case ( decrement (SingActive SingReqStr) q'
                   , decrementLemma (\case{}) (SingActive SingReqStr) SingTerminal q'
                   , decrementLemma (\case{}) (SingActive SingReqStr) (SingActive SingReqInt) q'
                   , decrementLemma (\case{}) (SingActive SingReqStr) (SingActive SingReqFib) q'
                   , decrementLemma2 (SingActive SingReqStr) q'
                   ) of
                ((q'', _), Refl, Refl, Refl, Refl) ->
                  SendResponse
                    (MsgResponseStr [c])
                    (go q'' i (fib, fib') (succ c))
        MsgDone ->
          case ( increment SingTerminal q
                 -- Lookup Terminal (Increment Terminal q) ≡ S (Lookup Terminal q)
               , incrementLemma2 SingTerminal q
               , incrementLemma (\case{}) SingTerminal (SingActive SingReqInt) q
               , incrementLemma (\case{}) SingTerminal (SingActive SingReqFib) q
               , incrementLemma (\case{}) SingTerminal (SingActive SingReqStr) q
               ) of
            (q', Refl, Refl, Refl, Refl) -> serverDone q'

    serverDone
      :: forall (q :: NatMap (State ReqType)).
         ( Lookup Terminal q ~ S Z
         , Lookup (Active ReqInt) q ~ Z
         , Lookup (Active ReqFib) q ~ Z
         , Lookup (Active ReqStr) q ~ Z
         )
      => SingNatMap q
      -> Peer ReqType (AsServer q) m ()
    serverDone q =
      case empty q of
        Refl -> ServerDone ()


-- | A server which collects all requests until the terminal request is sent,
-- then responds to all `ReqInt` and then to all `ReqPrime` and finally all
-- `ReqStr` requests.
--
-- It is called `Max` because it behaves like the client was pipelining maximum
-- number of requests before waiting for any response.
--
serverMax
  :: forall m. Peer ReqType (AsServer Empty) m ()
serverMax =
    serverAwaitLoop SingEmpty
  where
    serverAwaitLoop
      :: forall (q :: NatMap (State ReqType)).
         Lookup Terminal q ~ Z
      => SingNatMap q
      -> Peer ReqType (AsServer q) m ()
    serverAwaitLoop q =
      AwaitRequest $ \case
        MsgRequestInt ->
          case ( increment (SingActive SingReqInt) q
                 -- Lookup Terminal (Increment (Active ReqInt) q) ≡ Lookup Terminal q
               , incrementLemma (\case{}) (SingActive SingReqInt) SingTerminal q
               ) of
            (q', Refl) -> serverAwaitLoop q'
        MsgRequestFib ->
          case ( increment (SingActive SingReqFib) q
               , incrementLemma (\case{}) (SingActive SingReqFib) SingTerminal q
               ) of
            (q', Refl) -> serverAwaitLoop q'
        MsgRequestStr ->
          case ( increment (SingActive SingReqStr) q
                 -- Lookup Terminal (Increment (Active ReqStr) q) ≡ Lookup Terminal q
               , incrementLemma (\case{}) (SingActive SingReqStr) SingTerminal q
               ) of
            (q', Refl) -> serverAwaitLoop q'
        MsgDone ->
          case ( increment SingTerminal q
                 -- Lookup Terminal (Increment Terminal q) ≡ S (Lookup Terminal q)
               , incrementLemma2 SingTerminal q
               ) of
            (q', Refl) ->
              case lookup (SingActive SingReqInt) q' of
                Succ _ -> serverSendInts q' 0
                Zero   ->
                  case lookup (SingActive SingReqFib) q' of
                    Succ _ -> serverSendFibs q' 0 1
                    Zero ->
                      case lookup (SingActive SingReqStr) q' of
                        Succ _ -> serverSendChars Refl q' 'a'
                        Zero   -> serverDone q'


    -- reply all `ReqInt` requests
    serverSendInts
      :: forall (q :: NatMap (State ReqType)) (n :: N).
         ( Lookup (Active ReqInt) q ~ S n 
         , Lookup Terminal q ~ S Z
         )
      => SingNatMap q
      -> Int
      -> Peer ReqType (AsServer q) m ()
    serverSendInts q i =
      case ( decrement (SingActive SingReqInt) q
             -- lemmas
           , -- Lookup Terminal (Decrement (Active ReqInt) q) ≡ Lookup Terminal q
             decrementLemma
               (\case {}) -- Active ReqInt ≢ Terminal
               (SingActive SingReqInt)
               SingTerminal
               q
           , --   Lookup (Active ReqInt)                            q  ≡ S n
             -- ⇒ Lookup (Active ReqInt) (Decrement (Active ReqInt) q) ≡ n
             decrementLemma2 (SingActive SingReqInt) q
           )of
        ((q', Succ (Succ _)), Refl, Refl) ->
          SendResponse
            (MsgResponseInt i)
            (serverSendInts q' (i + 1))

        ((q', Succ Zero), Refl, Refl) ->
          SendResponse
            (MsgResponseInt i)
            (case lookup (SingActive SingReqFib) q' of
               Succ{} -> serverSendFibs q' 0 1
               Zero   ->
                 case lookup (SingActive SingReqStr) q' of
                   Succ _ -> serverSendChars Refl q' 'a'
                   Zero   -> serverDone q'
            )


    -- reply all `ReqFib` requests, incrementally computing Fibonacci numbers
    serverSendFibs
      :: forall (q :: NatMap (State ReqType)) (n :: N).
         ( Lookup (Active ReqFib) q ~ S n
         , Lookup (Active ReqInt) q ~ Z
         , Lookup Terminal q ~ S Z
         )
      => SingNatMap q
      -> Int -- ^ current Fibonacci number
      -> Int -- ^ next Fibonacci number
      -> Peer ReqType (AsServer q) m ()
    serverSendFibs q !fib fib' =
      case ( decrement (SingActive SingReqFib) q
             -- lemmas
           , -- Lookup Terminal (Decrement Active ReqFib q) ≡ Lookup Terminal q
             decrementLemma
               (\case {})
               (SingActive SingReqFib)
               SingTerminal
               q
           , -- Lookup (Active ReqFib) (Decrement (Active ReqInt) q) ≡ Lookup (Active ReqFib) q
             decrementLemma
               (\case {}) -- Active ReqFib ≢ Active ReqInt
               (SingActive SingReqFib)
               (SingActive SingReqInt)
               q
           , --   Lookup (Active ReqFib)                            q  ≡ S n
             -- ⇒ Lookup (Active ReqFib) (Decrement (Active ReqFib) q) ≡ n
             decrementLemma2
               (SingActive SingReqFib)
               q
           ) of
        ((q', Succ (Succ _)), Refl, Refl, Refl) ->
          SendResponse
            (MsgResponseFib fib)
            (serverSendFibs q' fib' (fib + fib'))

        ((q', Succ Zero), Refl, Refl, Refl) ->
          SendResponse
            (MsgResponseFib fib)
            -- TODO
            -- I shouldn't need to do the lookup
            (case lookup (SingActive SingReqStr) q' of
              Succ _ -> serverSendChars Refl q' 'a'
              Zero   -> serverDone q')
             


    -- reply all `ReqStr` requests
    serverSendChars
      :: forall (q :: NatMap (State ReqType)) (n :: N).
         ( Lookup Terminal q ~ S Z
         , Lookup (Active ReqInt) q ~ Z
         , Lookup (Active ReqFib) q ~ Z
         , Lookup (Active ReqStr) q ~ S n
         )
      => (Lookup (Active ReqInt) q :~: Z)
      -> SingNatMap q
      -> Char
      -> Peer ReqType (AsServer q) m ()
    serverSendChars Refl q c =
      case
        ( decrement
            (SingActive SingReqStr)
            q

          -- lemmas
        , -- Lookup (Active ReqInt) (Decrement (Active ReqStr) q) ≡ Lookup (Active ReqInt) q
          decrementLemma
            (\case {}) -- Active ReqStr ≢ Active ReqInt
            (SingActive SingReqStr)
            (SingActive SingReqInt)
            q
        , -- Lookup (Active ReqFib) (Decrement (Active ReqStr) q) ≡ Lookup (Active ReqFib) q
          decrementLemma
            (\case {}) -- Active ReqStr ≢ Active ReqFib
            (SingActive SingReqStr)
            (SingActive SingReqFib)
            q
        , -- Lookup Terminal (Decrement (Active ReqFib) q) ≡ Lookup Terminal q
          decrementLemma
            (\case {}) -- Active ReqStr ≢ Terminal
            (SingActive SingReqStr)
            SingTerminal
            q
        , --   Lookup (Active ReqStr)                            q  ≡ S n
          -- ⇒ Lookup (Active ReqStr) (Decrement (Active ReqStr) q) ≡ n
          decrementLemma2
            (SingActive SingReqStr)
            q
        ) of
        ((q', Succ (Succ _)), Refl, Refl, Refl, _) ->
          SendResponse
            (MsgResponseStr [c])
            (case lookup (SingActive SingReqStr) q' of
               Succ _ -> serverSendChars Refl q' (succ c)
               Zero -> serverDone q'
            )

        ((q', Succ Zero), Refl, Refl, Refl, Refl) ->
          SendResponse
            (MsgResponseStr [c])
            (serverDone q')

    -- terminate
    serverDone
      :: forall (q :: NatMap (State ReqType)).
         ( Lookup Terminal q ~ S Z
         , Lookup (Active ReqInt) q ~ Z
         , Lookup (Active ReqFib) q ~ Z
         , Lookup (Active ReqStr) q ~ Z
         )
      => SingNatMap q
      -> Peer ReqType (AsServer q) m ()
    serverDone q =
      case empty q of
        Refl -> ServerDone ()

-- NOTE: the instances are needed to eliminate some of the cases, but
-- GHC is not smart enough to figure it out, and places a warning
-- `Redundant constraints`, hence we use `-Wno-redundant-constraints`.
empty :: forall (q :: NatMap (State ReqType)).
         ( Lookup Terminal q ~ S Z
         , Lookup (Active ReqInt) q ~ Z
         , Lookup (Active ReqFib) q ~ Z
         , Lookup (Active ReqStr) q ~ Z
         )
      => SingNatMap q
      -> q :~: (Cons (Terminal :-> S Z) Empty :: NatMap (State ReqType))
empty (SingCons Refl SingTerminal (Succ Zero) SingEmpty) = Refl

empty (SingCons Refl SingTerminal (Succ Zero) (SingCons _ s _ _))
  = case s of {}
empty (SingCons _ (SingActive a) _ _)
  = case a of {}
