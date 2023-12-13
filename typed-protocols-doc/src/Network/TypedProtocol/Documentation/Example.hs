module Network.TypedProtocol.Documentation.Example
(
  -- * Definitions for convenient interactive testing
  testProtocolDescription
, testProtocolHtml
, testProtocolHtmlString
, testProtocolHtmlFile
)
where

import Network.TypedProtocol.Documentation.TestProtocolTH

-- $> :load Network.TypedProtocol.Documentation.TestProtocolTH

-- $> testProtocolHtmlFile "protocol.html"
