{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.TypedProtocol.Documentation.Text
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Knob as Knob
import Data.Maybe
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LText
import Data.Word
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import System.IO (IOMode (..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

import Network.TypedProtocol.Documentation.GraphViz
import Network.TypedProtocol.Documentation.Types

import Data.SerDoc.Class
import Data.SerDoc.Info

renderDescriptions :: [Description] -> LText.Text
renderDescriptions = LText.toLazyText $ mconcat $ map $ \(Description h) ->
    let (docs :: [Haddock.DocH () String]) =
          map (Haddock.toRegular . Haddock._doc . Haddock.parseParas Nothing) h
    in mconcat $ map renderHaddock docs

renderHaddock :: Haddock.DocH mod String -> LText.Builder
renderHaddock Haddock.DocEmpty = ""
renderHaddock (Haddock.DocAppend a b) = renderHaddock a <> renderHaddock b
renderHaddock (Haddock.DocString str) = LText.fromString str
renderHaddock (Haddock.DocParagraph a) = renderHaddock a <> "\n"
renderHaddock (Haddock.DocIdentifier i) = LText.fromString i
renderHaddock (Haddock.DocIdentifierUnchecked _) = "**unchecked**"
renderHaddock (Haddock.DocModule (Haddock.ModLink label _)) = LText.fromString label
renderHaddock (Haddock.DocWarning a) = renderHaddock a
renderHaddock (Haddock.DocEmphasis a) = renderHaddock a
renderHaddock (Haddock.DocMonospaced a) = renderHaddock a
renderHaddock (Haddock.DocBold a) = renderHaddock a
renderHaddock (Haddock.DocUnorderedList items) = mconcat [ "- " <> renderHaddock item <> "\n" | item <- items ]
renderHaddock (Haddock.DocOrderedList items) = mconcat [ "# " <> renderHaddock item <> "\n" | item <- items ]
renderHaddock (Haddock.DocDefList items) =
  mconcat
    [ renderHaddock title <> ": " <> renderHaddock body <> "\n"
    | (title, body) <- items
    ]
renderHaddock (Haddock.DocCodeBlock a) = renderHaddock a
renderHaddock (Haddock.DocHyperlink (Haddock.Hyperlink url a)) =
  renderHaddock a <> "(" <> LText.fromString url <> ")"
renderHaddock (Haddock.DocPic (Haddock.Picture url title)) =
  "[" <> renderHaddock a <> "](" <> LText.fromString url <> ")"
renderHaddock (Haddock.DocMathInline str) = LText.fromString str
renderHaddock (Haddock.DocMathDisplay str) = LText.fromString str
renderHaddock (Haddock.DocAName str) = LText.fromString str
renderHaddock (Haddock.DocProperty str) = LText.fromString str
renderHaddock (Haddock.DocExamples examples) =
  mconcat
    [ "> " <> LText.fromString expr <> "\n" <>
      mconcat [ LText.fromString ln <> "\n" | ln <- results ]
    | Haddock.Example expr results <- examples
    ]
renderHaddock (Haddock.DocHeader (Haddock.Header level a)) = do
  let h = LText.replicate level "#" <> " "
  h $ renderHaddock a
renderHaddock (Haddock.DocTable (Haddock.Table headerRows bodyRows)) = do
  mconcat $
    [ mconcat
        [ renderHaddock body
        | Haddock.TableCell _colspan _rowspan body
        <- Haddock.tableRowCells row
        ]
    | row <- headerRows
    ]
    ++
    [ "-----" ]
    ++
    [ mconcat
        [ renderHaddock body
        | Haddock.TableCell _colspan _rowspan body
        <- Haddock.tableRowCells row
        ]
    | row <- bodyRows
    ]

data TOC a = TOC a [TOC a]
  deriving (Show, Eq, Ord)

renderState :: String -> [MessageDescription codec] -> (StateRef, [Description], AgencyID) -> LText.Builder
renderState _ _ (AnyState, _, _) =
  return ()
renderState protoName msgs (State stateName, descriptions, agency) =
  mconcat
    [ "# " <> LText.fromString stateName <> "\n"
    , renderDescriptions descriptions
    H.p $ do
      "Agency: "
      case agency of
        ClientAgencyID -> H.strong ! HA.class_ "client-agency" $ "client"
        ServerAgencyID -> H.strong ! HA.class_ "server-agency" $ "server"
        NobodyAgencyID -> H.strong ! HA.class_ "nobody-agency" $ "nobody"
    unless (null messagesFromHere) $ do
      H.h4 "Messages from here:"
      H.ul $ do
        forM_ messagesFromHere $ \msg -> do
          H.li $ do
            H.strong $
              H.a ! HA.href (H.stringValue ("#" ++ messageID protoName (messageName msg))) $ H.string (messageName msg)
            " (to "
            formatStateRef protoName (messageToState msg)
            ")"
    unless (null messagesToHere) $ do
      H.h4 "Messages to here:"
      H.ul $ do
        forM_ messagesToHere $ \msg -> do
          H.li $ do
            H.strong $
              H.a ! HA.href (H.stringValue ("#" ++ messageID protoName (messageName msg))) $ H.string (messageName msg)
            " (from "
            formatStateRef protoName (messageFromState msg)
            ")"
  where
    messagesFromHere = filter ((matchState stateName) . messageFromState) msgs
    messagesToHere = filter ((matchState stateName) . messageToState) msgs

    matchState :: String -> StateRef -> Bool
    matchState _ AnyState = True
    matchState a (State b) = a == b

formatStateRef :: String -> StateRef -> Text
formatStateRef _ AnyState =
  H.span "any state"
formatStateRef protoName (State name) =
  H.a ! HA.href (H.stringValue ("#" ++ stateID protoName name)) $ H.string name

messageID :: String -> String -> String
messageID protoName msgName = protoName ++ "_message_" ++ msgName

messageTOC :: String -> MessageDescription codec -> TOC (String, String)
messageTOC protoName msg =
  TOC (messageName msg, messageID protoName (messageName msg)) []

formatFieldSize :: FieldSize -> String
formatFieldSize (FixedSize n) = show n
formatFieldSize (VarSize var) = var
formatFieldSize UnknownSize = "VARIABLE"
formatFieldSize EnumSize = "ENUM"
formatFieldSize (RangeSize lo hi) = "(" ++ formatFieldSize lo ++ " .. " ++ formatFieldSize hi ++ ")"
formatFieldSize (BinopSize FSPlus a b) = "(" ++ formatFieldSize a ++ " + " ++ formatFieldSize b ++ ")"
formatFieldSize (BinopSize FSMul a b) = "(" ++ formatFieldSize a ++ " * " ++ formatFieldSize b ++ ")"
formatFieldSize (BinopSize FSMax a b) = "MAX(" ++ formatFieldSize a ++ ", " ++ formatFieldSize b ++ ")"
formatFieldSize (BinopSize FSMin a b) = "MIN(" ++ formatFieldSize a ++ ", " ++ formatFieldSize b ++ ")"

renderMessage :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
              => String -> MessageDescription codec -> Text
renderMessage protoName msg =
  H.div ! HA.class_ "message" $ do
    H.h3 ! HA.id (H.stringValue (messageID protoName (messageName msg))) $ H.string (messageName msg)
    renderDescriptions (messageDescription msg)
    H.h4 $ "State Transition"
    H.p $ do
      formatStateRef protoName (messageFromState msg)
      " -> "
      formatStateRef protoName (messageToState msg)
    unless (null $ messagePayload msg) $ do
      H.h4 "Payload"
      H.ul $ do
        forM_ (messagePayload msg) $ H.li . H.string
    H.h4 "Serialization Format"
    fieldSpecToHTML (messageInfo msg)
  
protocolTOC :: ProtocolDescription codec -> TOC (String, String)
protocolTOC proto =
  let protoName = protocolName proto
  in
    TOC (protoName, protoName)
      [ TOC ("States", protoName ++ "_states")
        [ stateTOC protoName stateName | (State stateName, _, _) <- protocolStates proto ]
      , TOC ("Messages", protoName ++ "_messages")
        [ messageTOC protoName msg | msg <- protocolMessages proto ]
      ]

protocolToSvgMem :: ProtocolDescription codec -> ByteString
protocolToSvgMem proto = unsafePerformIO $ do
  k <- Knob.newKnob ""
  Knob.withFileHandle k "unknown" WriteMode $ \h ->
    hProtocolToSVG proto h
  Knob.getContents k

renderDiagramSvg :: ProtocolDescription codec -> Text
renderDiagramSvg proto = do
  let protoSvg = protocolToSvgMem proto
      uri = "data:image/svg+xml;base64," <> Base64.encode protoSvg
  H.img ! HA.src (H.unsafeByteStringValue uri)
        ! HA.class_ "state-diagram"

renderProtocol :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
               => ProtocolDescription codec -> Text
renderProtocol proto = do
  let protoName = protocolName proto
      msgs = protocolMessages proto
  H.section ! HA.class_ "protocol" $ do
    H.h1 ! HA.id (H.stringValue protoName) $ H.string protoName
    "Version ID: "
    H.code $ H.string (protocolIdentifier proto)
    renderDescriptions (protocolDescription proto)
    H.section $ do
      H.h2 ! HA.id (H.stringValue $ protoName ++ "_state_diagram") $ "State Diagram"
      renderDiagramSvg proto
    H.section $ do
      H.h2 ! HA.id (H.stringValue $ protoName ++ "_states") $ "States"
      mconcat <$> mapM (renderState protoName msgs) (protocolStates proto)
    H.section $ do
      H.h2 ! HA.id (H.stringValue $ protoName ++ "_messages") $ "Messages"
      mconcat <$> mapM (renderMessage protoName) msgs

fieldSpecToHTML :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                => FieldInfo codec -> Text
fieldSpecToHTML fi = do
  forM_ (fieldSpecAnnotations fi) (H.p . H.string)
  fromMaybe "" $ subfieldsToHTML (compoundField "" [("", fi)])

fieldSpecAnnotations :: FieldInfo codec -> [String]
fieldSpecAnnotations (AnnField ann fi) =
  ann : fieldSpecAnnotations fi
fieldSpecAnnotations _ = []

subfieldsToHTML :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                => FieldInfo codec -> Maybe Text
subfieldsToHTML (AnnField _ fi) =
  subfieldsToHTML fi
subfieldsToHTML (AliasField afi) =
  subfieldsToHTML (aliasFieldTarget afi)
subfieldsToHTML (CompoundField cfi) = Just $ do
  H.table $ do
    mapM_ subfieldToTextTR (compoundFieldSubfields cfi)
subfieldsToHTML _ = Nothing

fieldTypeToText :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                => FieldInfo codec -> Text
fieldTypeToText (AnnField _ fi) =
  fieldTypeToText fi
fieldTypeToText (AliasField fi) = do
  H.strong $ H.string (aliasFieldName fi)
  H.br
  H.em "This type is an alias for: "
  fieldTypeToText (aliasFieldTarget fi)
fieldTypeToText (ListField fi) = do
  H.strong $ do
    "["
    H.string (shortFieldType (listElemInfo fi))
    "]"
  H.br
  H.em $ "#items: "
  H.string $ formatFieldSize $ listSize fi
  H.br
  H.em $ "item type: "
  fieldTypeToText (listElemInfo fi)
  maybe "" (H.br <>) $
    subfieldsToHTML (listElemInfo fi)
fieldTypeToText (ChoiceField fi) = do
  H.em "Choice"
  let choiceLabel = case choiceCondition fi of
        IndexField ref -> H.string ref
        IndexFlag ref mask -> H.string ref <> " & " <> H.string (printf "0x%04x" mask)
  H.table $ do
    H.tr $ do
      H.th choiceLabel
      H.th "size"
      H.th "type"
    sequence_ $
          [ H.tr $ do
              H.td ! HA.class_ "choice-value" $ H.string (show n)
              H.td ! HA.class_ "field-size" $ do
                H.string $ formatFieldSize (fieldSize optInfo)
              H.td $ do
                fieldTypeToText optInfo
                fromMaybe "" $ subfieldsToHTML optInfo
          | (n :: Int, optInfo) <- zip [0,1..] (choiceFieldAlternatives fi)
          ]
fieldTypeToText (EnumField fi) = do
  H.strong $ H.string (enumFieldType fi)
  H.em " (enum)"
  H.table $ do
    H.tr $ do
      H.th "value"
      H.th "name"
    sequence_ $
        [ H.tr $ do
            H.td ! HA.class_ "enum-value" $ H.string (show val)
            H.td $ H.string name
        | (val, name) <- enumFieldValues fi
        ]
fieldTypeToText (SumField fi) = do
  H.strong $ H.string (sumFieldType fi)
  H.em " (union)"
  case sumFieldAlternatives fi of
    [(_name, sfi)] ->
      H.div $ fieldTypeToText sfi
    sfis ->
      H.table $ do
        H.tr $ do
          H.th "name"
          H.th "value"
        sequence_ $
            [ H.tr $ do
                H.td $ H.string name
                H.td ! HA.class_ "sum-value" $ fieldTypeToText sfi
            | (name, sfi) <- sfis
            ]
fieldTypeToText fi =
  H.strong . H.string . fieldType $ fi

subfieldToTextTR :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                 => SubfieldInfo codec -> Text
subfieldToTextTR sfi =
  case subfieldsToHTML (subfieldInfo sfi) of
    Nothing -> do
      H.tr $ do
        H.th ! HA.colspan "2" $ H.string (subfieldName sfi)
      H.tr $ do
        H.td ! HA.class_ "field-size" $ H.string $ formatFieldSize (fieldSize (subfieldInfo sfi))
        H.td $ fieldTypeToText (subfieldInfo sfi)
    Just sfiText -> do
      H.tr $ do
        H.th ! HA.colspan "2" $ H.string (subfieldName sfi)
      H.tr $ do
        H.td ! HA.rowspan "2" ! HA.class_ "field-size" $ do
          H.string $ formatFieldSize (fieldSize (subfieldInfo sfi))
        H.td ! HA.colspan "2" $ fieldTypeToText (subfieldInfo sfi)
      H.tr $ do
        H.td ! HA.colspan "2" $ sfiText

wrapDocument :: Text -> Text
wrapDocument body = do
  H.docType
  H.html $ do
    H.head $ do
      H.style $ do
        "html { font-family: sans-serif; }"
        "body { max-width: 60rem; margin-left: auto; margin-right: auto; padding: 1rem; }"
        "h1 { font-size: 3rem; }"
        "h2 { font-size: 2rem; }"
        "h3 { font-size: 1.5rem; }"
        "h4 { font-size: 1.25rem; }"
        "h5 { font-size: 1.1rem; }"
        "h6 { font-size: 1rem; }"
        "div.state, div.message {"
        " background-color: #EEE;"
        " padding: 0.125rem 1rem; "
        " margin: 1rem 0 1rem 0;"
        "}"
        "table { "
        "border-collapse: collapse;"
        "margin-top: 0.25rem;"
        "margin-bottom: 0.25rem;"
        "}"
        "table td, table th {"
        "  border: solid 1px black;"
        "  text-align: left;"
        "  vertical-align: top;"
        "  padding: 0.25rem;"
        "  background-color: white; "
        "}"
        "table th {"
        "  background-color: #DDD"
        "}"
        ".choice-value,"
        ".enum-value,"
        ".field-size {"
        "  text-align: right;"
        "  width: 4rem;"
        "}"
        ".toc>.toc {"
        "  padding-left: 2rem;"
        "}"
        ".state-diagram {"
        "  padding: 2rem;"
        "  width: 60%;"
        "}"
        ".client-agency {"
        "  color: brown;"
        "}"
        ".server-agency {"
        "  color: blue;"
        "}"
    H.body body

renderProtocolDescriptions :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                           => [ProtocolDescription codec] -> Text
renderProtocolDescriptions protos =
  mconcat $
    tocText : protoTexts
  where
    tocText = H.div ! HA.class_ "toc-master" $ do
                H.h1 "Table Of Contents"
                renderTOC $
                  TOC ("Protocols", "") $
                  map protocolTOC protos
    protoTexts = map renderProtocol protos

