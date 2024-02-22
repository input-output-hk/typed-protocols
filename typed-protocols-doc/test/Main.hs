module Main (main) where

import qualified Network.TypedProtocol.Tests.Documentation as Documentation

import Test.Tasty

main :: IO ()
main = defaultMain Documentation.tests
