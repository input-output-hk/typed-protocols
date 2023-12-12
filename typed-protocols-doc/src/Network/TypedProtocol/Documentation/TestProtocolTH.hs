{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.TypedProtocol.Documentation.TestProtocolTH
where

import Network.TypedProtocol.Documentation.Html
import Network.TypedProtocol.Documentation.Types
import Network.TypedProtocol.Documentation.TH
import Network.TypedProtocol.Documentation.TestProtocol

import Data.SerDoc.Class
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty

testProtocolDescription :: ProtocolDescription (TestCodec ())
testProtocolDescription = $(describeProtocol ''TestProtocol [''()] ''TestCodec [''()])

testProtocolHtmlString :: String
testProtocolHtmlString =
  Pretty.renderHtml . wrapDocument $
    renderProtocolDescriptions [testProtocolDescription]

testProtocolHtmlFile :: FilePath -> IO ()
testProtocolHtmlFile path =
  writeFile path testProtocolHtmlString

testProtocolHtml :: Text
testProtocolHtml =
  LText.toStrict . renderHtml . wrapDocument $
    renderProtocolDescriptions [testProtocolDescription]
