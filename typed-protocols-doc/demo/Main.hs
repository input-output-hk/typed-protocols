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
import Network.TypedProtocol.Documentation.TestProtocol
import Data.SerDoc.Class

main :: IO ()
main = defaultMain
  [ $(describeProtocol ''TestProtocol [''()] ''TestCodec [''()])
  ]
