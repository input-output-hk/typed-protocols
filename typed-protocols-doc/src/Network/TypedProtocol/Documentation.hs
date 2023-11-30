{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Network.TypedProtocol.Documentation
( 
  -- * Documentation Generators
  module M

, protocolToSVGFile
, protocolToDotFile
)
where

import Network.TypedProtocol.Documentation.Types as M
import Network.TypedProtocol.Documentation.TH as M
import Network.TypedProtocol.Documentation.GraphViz
