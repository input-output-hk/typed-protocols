module Main (main) where

import Test.Tasty

import Network.TypedProtocol.PingPong.Tests qualified as PingPong
import Network.TypedProtocol.ReqResp.Tests qualified as ReqResp

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "typed-protocols"
  [ PingPong.tests
  , ReqResp.tests
  ]

