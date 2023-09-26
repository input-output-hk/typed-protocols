{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Network.TypedProtocol.Trans.Wedge where

import           Network.TypedProtocol.Core

import qualified Network.TypedProtocol.Peer.Client as Client
import qualified Network.TypedProtocol.PingPong.Type as PingPong


-- | A [wedge](https://hackage.haskell.org/package/smash-0.1.2/docs/Data-Wedge.html)
-- sum of two protocols.
--
-- One can interleave both protocols using protocol pipelining.  Termination
-- must be done by terminating one of the protocols.
--
data Wedge ps (stIdle :: ps) ps' (stIdle' :: ps') where
    StIdle :: Wedge ps stIdle ps' stIdle'
    StFst  :: ps  -> Wedge ps stIdle ps' stIdle'
    StSnd  :: ps' -> Wedge ps stIdle ps' stIdle'


data SingWedge (st ::  Wedge ps (stIdle :: ps) ps' (stIdle' :: ps')) where
    SingStIdle :: SingWedge StIdle
    SingStFst  :: StateToken st
               -> SingWedge (StFst st)
    SingStSnd  :: StateToken st'
               -> SingWedge (StSnd st')

instance Show (SingWedge StIdle) where
    show SingStIdle    = "SingStIdle"
instance Show (StateToken st) => Show (SingWedge (StFst st)) where
    show (SingStFst s) = "SingStFst " ++ show s
instance Show (StateToken st) => Show (SingWedge (StSnd st)) where
    show (SingStSnd s) = "SingStSnd " ++ show s

instance StateTokenI StIdle where
    stateToken = SingStIdle
instance StateTokenI st => StateTokenI (StFst st) where
    stateToken = SingStFst (stateToken @st)
instance StateTokenI st => StateTokenI (StSnd st) where
    stateToken = SingStSnd (stateToken @st)


-- | A Singleton type which allows to pick the starting protocol state.
--
data SingStart (st :: Wedge ps stIdle ps' stIdle') where
    AtFst :: SingStart (StFst stIdle)
    AtSnd :: SingStart (StSnd stIdle)


-- Note: This does not require @(Protocol ps, Protocol ps')@, ghc is not
-- requiring class constraints for associated type families / data types the
-- same way as for terms.
--
instance Protocol (Wedge ps (stIdle :: ps) ps' (stIdle' :: ps')) where

    data Message  (Wedge ps (stIdle :: ps) ps' (stIdle' :: ps')) from to where
      -- | Signal that starts one of the protocols.
      --
      MsgStart :: SingStart st
               -> Message (Wedge ps stIdle ps' stIdle')
                          StIdle st

      -- | Embed any @ps@ message.
      --
      MsgFst      :: Message ps  st st'
                  -> Message (Wedge ps stIdle ps' stIdle')
                             (StFst st) (StFst st')


      -- | Embed any @ps'@ message.
      MsgSnd      :: Message ps' st st'
                  -> Message (Wedge ps stIdle ps' stIdle')
                             (StSnd st) (StSnd st')

      -- | Switch from @ps@ to @ps'@.
      --
      MsgFstToSnd :: Message (Wedge ps stIdle ps' stIdle')
                             (StFst stIdle) (StSnd stIdle')

      -- | Switch from @ps'@ to @ps@.
      --
      MsgSndToFst :: Message (Wedge ps stIdle ps' stIdle')
                             (StSnd stIdle') (StFst stIdle)


    type StateAgency StIdle     = ClientAgency
    type StateAgency (StFst st) = StateAgency st
    type StateAgency (StSnd st) = StateAgency st

    type StateToken = SingWedge


type PingPong2 = Wedge PingPong.PingPong PingPong.StIdle
                       PingPong.PingPong PingPong.StIdle


pingPong2Client :: Client.Client PingPong2 NonPipelined Client.Z StIdle m ()
pingPong2Client =
    Client.Yield (MsgStart AtFst)
  $ Client.Yield (MsgFst PingPong.MsgPing)
  $ Client.Await $ \(MsgFst PingPong.MsgPong) ->
    Client.Yield MsgFstToSnd
  $ Client.Yield (MsgSnd PingPong.MsgPing)
  $ Client.Await $ \(MsgSnd PingPong.MsgPong) ->
  -- terminate, through the second protocol
    Client.Yield (MsgSnd PingPong.MsgDone)
  $ Client.Done ()


pingPong2Client' :: forall m. Client.Client PingPong2 (Pipelined ()) Client.Z StIdle m ()
pingPong2Client' =
    --
    -- Pipeline first protocol
    --

      Client.YieldPipelined (MsgStart AtFst)
                            (Client.ReceiverDone ())
    $ Client.YieldPipelined (MsgFst PingPong.MsgPing)
                            (Client.ReceiverAwait (\(MsgFst PingPong.MsgPong) -> Client.ReceiverDone ()))

    --
    -- Pipeline second protocol
    --

    $ Client.YieldPipelined MsgFstToSnd
                            (Client.ReceiverDone ())
    $ Client.YieldPipelined (MsgSnd PingPong.MsgPing)
                            (Client.ReceiverAwait (\(MsgSnd PingPong.MsgPong) -> Client.ReceiverDone ()))

    --
    -- Collect responses from the first protocol
    --

    $ Client.Collect Nothing $ \() -> -- collect transition pushed by `MsgStartFst`
      Client.Collect Nothing $ \() -> -- collect reply received with `MsgFst MsgPong`

    --
    -- Collect responses from the second protocol
    --

      Client.Collect Nothing $ \() -> -- collect transition pushed by MsgFstToSnd
      Client.Collect Nothing $ \() -> -- collect reply received with `MsgSnd MsgPong`

    --
    -- Terminate the protocol
    --

      Client.Yield (MsgSnd PingPong.MsgDone)
    $ Client.Done ()
