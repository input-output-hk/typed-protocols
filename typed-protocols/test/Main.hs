module Main (main) where

import Test.Tasty

import Network.TypedProtocol.PingPong.Tests qualified as PingPong
import Network.TypedProtocol.ReqResp.Tests qualified as ReqResp
import Network.TypedProtocol.ReqResp3.Tests qualified as ReqResp3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "typed-protocols"
  [ PingPong.tests
  , ReqResp.tests
  , ReqResp3.tests
  ]

