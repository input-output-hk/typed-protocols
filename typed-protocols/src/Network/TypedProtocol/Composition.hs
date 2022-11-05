{-# LANGUAGE GADTs            #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}

module Network.TypedProtocol.Composition where

import           Data.Singletons

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer

data ProtocolArrow ps pr st st' where
    Send :: forall ps pr (st :: ps) (st' :: ps).
            ( SingI st
            , SingI st'
            , ActiveState st
            )            
         => WeHaveAgencyProof pr st
         -> Message ps st st'
         -> ProtocolArrow ps pr st st'

    -- as with `Peer` we use existential variables, so the remote peer decides
    -- the next state.  `Recv` is recursive.
    Recv :: forall ps pr (st :: ps) (st'' :: ps).
            ( SingI st
            , ActiveState st
            )
         => TheyHaveAgencyProof pr st
         -> (forall (st' :: ps). Message ps st st' -> ProtocolArrow ps pr st' st'')
         -> ProtocolArrow ps pr st st''

    Compose :: ProtocolArrow ps pr st st'
            -> ProtocolArrow ps pr st' st''
            -> ProtocolArrow ps pr st st''


compose :: ProtocolArrow ps pr st  st'
        -> ProtocolArrow ps pr st' st''
        -> ProtocolArrow ps pr st  st''
compose = Compose


arrowToPeer :: ProtocolArrow ps pr st st'
            -> Peer ps pr NonPipelined Empty st m stm a

arrowToPeer (Send proof msg) = Yield proof msg undefined
                                               -- this is not possible just
                                               -- because of limitation of
                                               -- `ProtocolArrow` which could
                                               -- be improved
arrowToPeer (Recv proof k)   = Await proof (\msg -> arrowToPeer (k msg))

arrowToPeer (Compose f g)    = undefined
                               -- this is fundamentally not possible
                               -- `arrowToPeer f` cannot be composed with
                               -- `arrowToPeer g`!


data SomeProtocolArrow ps pr st where
    SomeProtocolArrow :: ProtocolArrow ps pr st st'
                      -> SomeProtocolArrow ps pr st

peerToArrow :: Peer ps pr NonPipelined Empty st m stm a
            -> SomeProtocolArrow ps pr st

peerToArrow (Yield proof msg k) =
    case peerToArrow k of
      SomeProtocolArrow k' ->
        SomeProtocolArrow (Compose (Send proof msg) k')

peerToArrow (Await proof k) =
    SomeProtocolArrow $ Recv proof $ \msg ->
      case peerToArrow (k msg) of
        SomeProtocolArrow k' -> undefined -- k'
        -- • Couldn't match type ‘st'0’ with ‘st'1’
        --   Expected: ProtocolArrow ps pr st' st'0
        --     Actual: ProtocolArrow ps1 pr st' st'1
        --     because type variable ‘st'1’ would escape its scope
        --   This (rigid, skolem) type variable is bound by
        --     a pattern with constructor:
        --       SomeProtocolArrow :: forall {ps1} ps2 (pr :: PeerRole) (st :: ps1)
        --                                   (st' :: ps1).
        --                            ProtocolArrow ps2 pr st st' -> SomeProtocolArrow ps2 pr st,
        --     in a case alternative
        --     at src/Network/TypedProtocol/Composition.hs:72:9-28
