module Network.TypedProtocol.Stateful.ReqResp.Examples where

import           Network.TypedProtocol.Stateful.ReqResp.Server
import           Network.TypedProtocol.Stateful.ReqResp.Type


fileRPCServer :: Monad m
              => (forall resp. FileAPI resp -> m resp)
              -- ^ execute `FileAPI` locally
              -> ReqRespServer FileAPI m ()
fileRPCServer run = ReqRespServer {
    reqRespServerDone = (),
    reqRespHandleReq = \req -> do
      resp <- run req
      return (resp, fileRPCServer run)
  }

-- | Example of a file API
--
simpleFileAPI :: Monad m => FileAPI resp -> m resp
simpleFileAPI (ReadFile filepath) = return filepath
simpleFileAPI (WriteFile _ _)     = return ()

simpleFileRPCServer :: Monad m => ReqRespServer FileAPI m ()
simpleFileRPCServer = fileRPCServer simpleFileAPI

