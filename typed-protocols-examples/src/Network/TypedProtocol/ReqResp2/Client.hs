{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}



module Network.TypedProtocol.ReqResp2.Client where

import           Network.TypedProtocol.ReqResp2.Type

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client


reqResp2Client :: forall req resp m.
                  ()
               => [Either req req]
               -> Client (ReqResp2 req resp) (Pipelined (Either resp resp)) Z StIdle m [Either resp resp]
reqResp2Client = send Zero
  where
    -- pipeline all the requests, either through `MsgReq` or `MsgReq'`.
    send :: forall (n :: N).
            Nat n
         -> [Either req req] -- requests to send
         -> Client (ReqResp2 req resp) (Pipelined (Either resp resp))  n StIdle m [Either resp resp]

    send !n (Left req : reqs) =
      YieldPipelined (MsgReq  req) receiver (send (Succ n) reqs)

    send !n (Right req : reqs) =
      YieldPipelined (MsgReq' req) receiver' (send (Succ n) reqs)

    send !n [] = collect n []


    receiver :: Receiver (ReqResp2 req resp) StBusy StIdle m (Either resp resp)
    receiver = ReceiverAwait (\(MsgResp resp) -> ReceiverDone (Left resp))


    receiver' :: Receiver (ReqResp2 req resp) StBusy' StIdle m (Either resp resp)
    receiver' = ReceiverAwait (\(MsgResp' resp) -> ReceiverDone (Right resp))


    -- collect all the responses
    collect :: Nat n
            -> [Either resp resp] -- all the responses received so far
            -> Client (ReqResp2 req resp) (Pipelined (Either resp resp)) n StIdle m [Either resp resp]

    collect Zero !resps = Yield MsgDone (Done (reverse resps))

    collect (Succ n) !resps =
      Collect Nothing $ \c  -> collect n (c : resps)



