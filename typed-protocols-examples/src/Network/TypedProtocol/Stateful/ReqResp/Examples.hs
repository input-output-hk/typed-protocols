{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.Stateful.ReqResp.Examples
  ( ReqRespStateCallbacks (..)
  , reqRespClientMap
  ) where

import           Data.Kind (Type)

import           Network.TypedProtocol.ReqResp.Type
import           Network.TypedProtocol.Stateful.ReqResp.Client


data ReqRespStateCallbacks (f :: ReqResp req resp -> Type) =
    ReqRespStateCallbacks {
        rrBusyToIdle :: f StBusy -> f StIdle
      , rrBusyToBusy :: f StBusy -> f StBusy
      , rrBusyToDone :: f StBusy -> f StDone
      }

reqRespClientMap
  :: forall req resp f m.
     Monad m
  => ReqRespStateCallbacks f
  -> f StBusy
  -> [req]
  -> ReqRespClient req resp f m ([resp], f StDone)
reqRespClientMap ReqRespStateCallbacks
                           { rrBusyToIdle
                           , rrBusyToBusy
                           , rrBusyToDone
                           } = go []
  where
    go :: [resp]
       -> f StBusy
       -> [req]
       -> ReqRespClient req resp f m ([resp], f StDone)
    go resps f []         = SendMsgDone f' (pure (reverse resps, f'))
      where
        f' = rrBusyToDone f
    go resps f (req:reqs) =
      SendMsgReq f req $ \f' resp ->
        ( return (go (resp:resps) (rrBusyToBusy f') reqs)
        , rrBusyToIdle f'
        )

