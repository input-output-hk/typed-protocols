{-# LANGUAGE FlexibleContexts #-}

module Network.TypedProtocol.Documentation.DefaultMain
where

import qualified Data.Text.Lazy as LText
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import Data.SerDoc.Class hiding (info)
import Data.Word
import qualified Network.TypedProtocol.Documentation.Html as HTML
import qualified Network.TypedProtocol.Documentation.Text as TextRender
import Network.TypedProtocol.Documentation.Types
import Options.Applicative
import Control.Monad
import System.FilePath

data MainOptions =
  MainOptions
    { moOutputFile :: Maybe FilePath
    , moOutputFormat :: OutputFormat
    , moListProtocols :: Bool
    }

data OutputFormat
  = OutputAuto
  | OutputText
  | OutputHtml
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat "auto" = return OutputAuto
parseOutputFormat "text" = return OutputText
parseOutputFormat "html" = return OutputHtml
parseOutputFormat _ = Nothing

pMainOptions :: Parser MainOptions
pMainOptions =
  MainOptions
    <$> option (Just <$> str)
          ( short 'o'
          <> value Nothing
          <> metavar "FILE"
          <> help "Output file (default: stdout)"
          )
    <*> option (maybeReader parseOutputFormat)
          ( short 'f'
          <> value OutputAuto
          <> metavar "FORMAT"
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
        render = getRenderer (moOutputFormat mainOptions) (moOutputFile mainOptions)
    write . render $ descriptions

getRenderer :: ( Codec codec
               , HasInfo codec (DefEnumEncoding codec)
               , HasInfo codec Word32
               )
            => OutputFormat
            -> Maybe FilePath
            -> [ProtocolDescription codec]
            -> String
getRenderer OutputAuto path =
  case takeExtension <$> path of
    Just "html" -> getRenderer OutputHtml path
    Just "htm" -> getRenderer OutputHtml path
    _ -> getRenderer OutputText path
getRenderer OutputHtml _ =
  Pretty.renderHtml . HTML.wrapDocument . HTML.renderProtocolDescriptions
getRenderer OutputText _ =
  LText.unpack . TextRender.renderProtocolDescriptions
