module Network.TypedProtocol.Documentation
( 
  module M

, protocolToSVGFile
, protocolToDotFile

, defaultMain
, testMain
)
where

import Network.TypedProtocol.Documentation.Types as M
import Network.TypedProtocol.Documentation.TH as M
import Network.TypedProtocol.Documentation.GraphViz
import Network.TypedProtocol.Documentation.DefaultMain
import Network.TypedProtocol.Documentation.Example

testMain :: IO ()
testMain = defaultMain [ testProtocolDescription ]
