{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main
where

import Network.TypedProtocol.Documentation
import DemoProtocol
import Data.SerDoc.Class

main :: IO ()
main = defaultMain
  [ $(describeProtocol ''DemoProtocol [''()] ''DemoCodec [''()])
  ]
