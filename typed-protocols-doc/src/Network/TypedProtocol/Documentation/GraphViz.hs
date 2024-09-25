{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.TypedProtocol.Documentation.GraphViz
where

import Data.Maybe
import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Attributes.Complete as Dot
import Data.Graph.Inductive.Graph (mkGraph, Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Text.Lazy as LText
import qualified Data.Map.Strict as Map
import System.IO (Handle)
import qualified Data.ByteString as BS

import Network.TypedProtocol.Documentation.Types

protocolToDot :: ProtocolDescription codec -> Dot.DotGraph Node
protocolToDot proto =
  Dot.graphToDot
          Dot.nonClusteredParams
            { Dot.fmtNode = \ (_, (name, agency)) ->
                stateToNode name agency
            , Dot.fmtEdge = \ (_, _, name) ->
                [ Dot.Label (Dot.StrLabel $ LText.pack name)
                ]
            , Dot.globalAttributes =
                [ Dot.GraphAttrs
                    [ Dot.FontSize 10.0
                    , Dot.LabelFontSize 10.0
                    ]
                , Dot.NodeAttrs
                    [ Dot.FontName "Noto Sans"
                    , Dot.LabelFontName "Noto Sans"
                    ]
                , Dot.EdgeAttrs
                    [ Dot.FontName "Noto Sans"
                    , Dot.LabelFontName "Noto Sans"
                    , Dot.FontSize 8.0
                    , Dot.LabelFontSize 8.0
                    ]
                ]
            }
          (mkGraph numberedStateInfos edges :: Gr (StateRef, AgencyID) String)
  where
    stateInfos = [ (name, agency)  | (name, _, agency) <- protocolStates proto ]
    stateNames = map fst stateInfos
    numberedStateInfos = zip [0::Int,1..] stateInfos
    numberedStateNames = zip [0::Int,1..] stateNames
    stateDict =
      Map.fromList $ map flipPair numberedStateNames

    agencyColor NobodyAgencyID = Dot.toColorList [Dot.X11Color Dot.Black]
    agencyColor ServerAgencyID = Dot.toColorList [Dot.X11Color Dot.Blue]
    agencyColor ClientAgencyID = Dot.toColorList [Dot.X11Color Dot.Brown]

    edges = catMaybes $ flip map (protocolMessages proto) $ \msg -> do
              fromIndex <- Map.lookup (messageFromState msg) stateDict
              toIndex <- Map.lookup (messageToState msg) stateDict
              return (fromIndex, toIndex, messageName msg)

    stateToNode AnyState _ =
      [ Dot.Label (Dot.StrLabel "any state")
      , Dot.Color (Dot.toColorList [Dot.X11Color Dot.Gray])
      ]
    stateToNode (State name) agency =
      [ Dot.Label (Dot.StrLabel $ LText.pack name)
      , Dot.Color (agencyColor agency)
      ]

flipPair :: (a, b) -> (b, a)
flipPair (x, y) = (y, x)

protocolToSVGFile :: ProtocolDescription codec -> FilePath -> IO FilePath
protocolToSVGFile proto =
  Dot.runGraphvizCommand Dot.Dot (protocolToDot proto) Dot.Svg

protocolToDotFile :: ProtocolDescription codec -> FilePath -> IO FilePath
protocolToDotFile proto =
  Dot.runGraphvizCommand Dot.Dot (protocolToDot proto) Dot.Canon

hProtocolToSVG :: ProtocolDescription codec -> Handle -> IO ()
hProtocolToSVG proto dst =
  Dot.graphvizWithHandle Dot.Dot (protocolToDot proto) Dot.Svg consume
  where
    consume src = BS.hGetContents src >>= BS.hPut dst
