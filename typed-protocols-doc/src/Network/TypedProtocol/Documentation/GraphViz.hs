{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.TypedProtocol.Documentation.GraphViz
where

import Control.Monad
import Data.Maybe
import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Attributes.Complete as Dot
import Data.Graph.Inductive.Graph (mkGraph, Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Map.Strict as Map
import System.IO (Handle)
import qualified Data.ByteString as BS

import Network.TypedProtocol.Documentation.Types

protocolToDot :: ProtocolDescription codec -> Dot.DotGraph Node
protocolToDot proto =
  Dot.graphToDot
          Dot.nonClusteredParams
            { Dot.fmtNode = \ (_, name) ->
                [ Dot.Label (Dot.StrLabel $ LText.fromStrict name)
                ]
            , Dot.fmtEdge = \ (_, _, name) ->
                [ Dot.Label (Dot.StrLabel $ LText.fromStrict name)
                ]
            }
          (mkGraph numberedStateNames edges :: Gr Text Text)
  where
    stateNames = [ Text.pack name | (name, _, _) <- protocolStates proto ]
    numberedStateNames = zip [0::Int,1..] stateNames
    stateDict =
      Map.fromList $ map flipPair numberedStateNames
    edges = catMaybes $ flip map (protocolMessages proto) $ \msg -> do
              fromIndex <- Map.lookup (Text.pack $ messageFromState msg) stateDict
              toIndex <- Map.lookup (Text.pack $ messageToState msg) stateDict
              return (fromIndex, toIndex, Text.pack $ messageName msg)

flipPair :: (a, b) -> (b, a)
flipPair (x, y) = (y, x)

protocolToSVGFile :: ProtocolDescription codec -> FilePath -> IO FilePath
protocolToSVGFile proto =
  Dot.runGraphviz (protocolToDot proto) Dot.Svg

protocolToDotFile :: ProtocolDescription codec -> FilePath -> IO FilePath
protocolToDotFile proto =
  Dot.runGraphviz (protocolToDot proto) Dot.Canon

hProtocolToSVG :: ProtocolDescription codec -> Handle -> IO ()
hProtocolToSVG proto dst =
  Dot.graphvizWithHandle Dot.Dot (protocolToDot proto) Dot.Svg consume
  where
    consume src = BS.hGetContents src >>= BS.hPut dst
