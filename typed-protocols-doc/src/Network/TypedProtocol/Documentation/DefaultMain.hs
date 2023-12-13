{-# LANGUAGE FlexibleContexts #-}

module Network.TypedProtocol.Documentation.DefaultMain
where

import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import Data.SerDoc.Class hiding (info)
import Data.Word
import Network.TypedProtocol.Documentation.Html
import Network.TypedProtocol.Documentation.Types
import Options.Applicative
import Control.Monad

data MainOptions =
  MainOptions
    { moOutputFile :: Maybe FilePath
    , moListProtocols :: Bool
    }

pMainOptions :: Parser MainOptions
pMainOptions =
  MainOptions
    <$> option (Just <$> str)
          ( short 'o'
          <> value Nothing
          <> metavar "FILE"
          <> help "Output file (default: stdout)"
          )
    <*> switch
          (  long "list-protocols"
          <> long "list"
          <> short 'l'
          <> help "Print a list of protocols and exit"
          )


defaultMain :: ( Codec codec
               , HasInfo codec (DefEnumEncoding codec)
               , HasInfo codec Word32
               ) => [ProtocolDescription codec] -> IO ()
defaultMain descriptions = do
  mainOptions <- execParser $ info (pMainOptions <**> helper) fullDesc
  if moListProtocols mainOptions then do
    forM_ descriptions $ \d -> do
      putStrLn (protocolName d)
  else do
    let write = maybe putStrLn writeFile $ moOutputFile mainOptions
    write (Pretty.renderHtml . wrapDocument . renderProtocolDescriptions $ descriptions)
