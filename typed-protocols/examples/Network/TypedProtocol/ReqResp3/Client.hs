{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.ReqResp3.Client
  ( clientPeer
  , clientPeerRoundRobin
  , Result (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (void)
import Control.Monad.Class.MonadSay
import Data.Proxy

import Network.TypedProtocol.ReqResp
import Network.TypedProtocol.ReqResp3.Type


data Result = ResInt Int | ResFib Int | ResStr String
  deriving (Show, Eq)

-- | A client which sends:
--
-- * m `ReqInt` requests, then
-- * n `ReqFib` requests, then
-- * o `ReqStr` requests.
--
clientPeer
  :: forall m.
     ( MonadLabelledSTM m
     , MonadTraceSTM m
     , MonadSay m
     )
  => Int
  -> Int
  -> Int
  -> Peer ReqType (AsClient Running) m [Result]
clientPeer m0 n0 o0 = Effect $ do
    v <- atomically $ do
      v <- newTVar []
      labelTVar v "res"
      traceTVar Proxy v (\_ a -> pure (TraceString ("res: " ++ show a)))
      pure v
    return $ clientInt m0 v
  where
    clientInt
      :: Int
      -> StrictTVar m [Result]
      -> Peer ReqType (AsClient Running) m [Result]
    clientInt m v
      | m > 0
      = SendRequest
          MsgRequestInt
          (\case MsgResponseInt a -> void $ atomically $ modifyTVar v (ResInt a:))
          (clientInt (m - 1) v)
      | otherwise
      = clientFib n0 v

    clientFib
      :: Int
      -> StrictTVar m [Result]
      -> Peer ReqType (AsClient Running) m [Result]
    clientFib m v
      | m > 0
      = SendRequest
          MsgRequestFib
          (\case MsgResponseFib a -> void $ atomically $ modifyTVar v (ResFib a:))
          (clientFib (m - 1) v)
      | otherwise
      = clientStr o0 v

    clientStr
      :: Int
      -> StrictTVar m [Result]
      -> Peer ReqType (AsClient Running) m [Result]
    clientStr m v
      | m > 0
      = SendRequest
          MsgRequestStr
          (\case MsgResponseStr a -> void $ atomically $ modifyTVar v (ResStr a:))
          (clientStr (m - 1) v)
      | otherwise
      = clientTerminated v

    clientTerminated
      :: StrictTVar m [Result]
      -> Peer ReqType (AsClient Running) m [Result]
    clientTerminated v =
      SendRequestDone
        MsgDone
        (Effect $ do
          res <- reverse <$> readTVarIO v
          return $ ClientDone res)


-- | A client which sends m batches of three requests: `ReqStr`, `ReqFib`,
-- `ReqInt`.
--
clientPeerRoundRobin
  :: forall m.
     ( MonadLabelledSTM m
     , MonadTraceSTM m
     , MonadSay m
     )
  => Int
  -> Peer ReqType (AsClient Running) m [Result]
clientPeerRoundRobin m0 = Effect $ do
    v <- atomically $ do
      v <- newTVar []
      labelTVar v "res"
      traceTVar Proxy v (\_ a -> pure (TraceString ("res: " ++ show a)))
      return v
    pure $ clientLoop m0 v
  where
    clientLoop
      :: Int
      -> StrictTVar m [Result]
      -> Peer ReqType (AsClient Running) m [Result]
    clientLoop m v | m > 0 =
        SendRequest
          MsgRequestStr
          (\case MsgResponseStr s -> void $ atomically $ modifyTVar v (ResStr s :))
      $ SendRequest
          MsgRequestFib
          (\case MsgResponseFib f -> void $ atomically $ modifyTVar v (ResFib f :))
      $ SendRequest
          MsgRequestInt
          (\case MsgResponseInt i -> void $ atomically $ modifyTVar v (ResInt i :))
          (clientLoop (m - 1) v)

      | otherwise
      = clientTerminated v
        
    clientTerminated
      :: StrictTVar m [Result]
      -> Peer ReqType (AsClient Running) m [Result]
    clientTerminated v =
      SendRequestDone MsgDone
         (Effect $ do
            say "readTVar:res"
            res <- reverse <$> readTVarIO v
            return (ClientDone res))
