module Network.TypedProtocol.ReqResp.PeerRole
  ( PeerRole (..)
  , SingPeerRole (..)
  ) where

import Data.Type.NatMap
import Network.TypedProtocol.ReqResp.Core

data SingPeerRole (pr :: PeerRole ps) where
  SingAsClient :: forall (cs :: ClientState).
                  SingClientState cs
               -> SingPeerRole (AsClient cs)
  SingAsServer :: forall ps (natMap :: NatMap (State ps)).
                  SingNatMap natMap
               -> SingPeerRole (AsServer natMap)
