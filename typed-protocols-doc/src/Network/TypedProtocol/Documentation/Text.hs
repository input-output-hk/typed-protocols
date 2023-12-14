{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.TypedProtocol.Documentation.Text
where

import Control.Monad
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LText
import Data.Word
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import Text.Printf
import Control.Monad.Writer
import Data.List (intersperse)

import Network.TypedProtocol.Documentation.Types

import Data.SerDoc.Class
import Data.SerDoc.Info

type Build = Writer LText.Builder ()

string :: String -> Build
string = tell . LText.fromString

newline :: Build
newline = string "\n"

p :: Build -> Build
p b = b >> newline

stringLine :: String -> Build
stringLine = p . string

h1 :: Build -> Build
h1 b =
  b >> newline >> tell (LText.fromLazyText $ LText.replicate l "=") >> newline
  where
    l = LText.length . LText.toLazyText . execWriter $ b

h2 :: Build -> Build
h2 b =
  b >> newline >> tell (LText.fromLazyText $ LText.replicate l "-") >> newline
  where
    l = LText.length . LText.toLazyText . execWriter $ b

h :: Int -> Build -> Build
h n b = do
  replicateM_ n $ string "#"
  string " "
  b
  newline

ul :: [Build] -> Build
ul items = forM_ items $ \item -> do
  tell "- "
  item
  newline

ol :: [(Int, Build)] -> Build
ol = mapM_ (uncurry renderItem)
  where
    renderItem :: Int -> Build -> Build
    renderItem n item = do
      string . show $ n
      tell ". "
      item
      newline

link :: String -> Build -> Build
link url label = do
  string "["
  label
  string "]("
  string url
  string ")"

renderDescriptions :: [Description] -> Build
renderDescriptions = mapM_ $ \(Description desc) -> do
    let (docs :: [Haddock.DocH () String]) =
          map (Haddock.toRegular . Haddock._doc . Haddock.parseParas Nothing) desc
    mapM_ (p . renderHaddock) docs

renderHaddock :: Haddock.DocH mod String -> Build
renderHaddock Haddock.DocEmpty = return ()
renderHaddock (Haddock.DocAppend a b) = renderHaddock a >> renderHaddock b
renderHaddock (Haddock.DocString str) = string str
renderHaddock (Haddock.DocParagraph a) = p (renderHaddock a)
renderHaddock (Haddock.DocIdentifier i) = string i
renderHaddock (Haddock.DocIdentifierUnchecked _) = string "**unchecked**"
renderHaddock (Haddock.DocModule (Haddock.ModLink label _)) = string label
renderHaddock (Haddock.DocWarning a) = p $ renderHaddock a
renderHaddock (Haddock.DocEmphasis a) = renderHaddock a
renderHaddock (Haddock.DocMonospaced a) = renderHaddock a
renderHaddock (Haddock.DocBold a) = renderHaddock a
renderHaddock (Haddock.DocUnorderedList items) = ul $ map renderHaddock items
renderHaddock (Haddock.DocOrderedList items) = ol $ map (\(n, item) -> (n, renderHaddock item)) items
renderHaddock (Haddock.DocDefList items) =
  ul
    [ renderHaddock title >> string ": " >> renderHaddock body
    | (title, body) <- items
    ]
renderHaddock (Haddock.DocCodeBlock a) = renderHaddock a
renderHaddock (Haddock.DocHyperlink (Haddock.Hyperlink url a)) =
  link url (maybe (string "") renderHaddock a)
renderHaddock (Haddock.DocPic (Haddock.Picture url title)) = do
  string "<image:"
  maybe (string url) string title
  string ">"
renderHaddock (Haddock.DocMathInline str) = string str
renderHaddock (Haddock.DocMathDisplay str) = p $ string str
renderHaddock (Haddock.DocAName str) = string str
renderHaddock (Haddock.DocProperty str) = string str
renderHaddock (Haddock.DocExamples examples) =
  forM_ examples $ \(Haddock.Example expr results) -> do
    p $ do
      string "$> "
      stringLine expr
      mapM_ stringLine results
renderHaddock (Haddock.DocHeader (Haddock.Header level a)) = do
  let renderH = case level of
                  1 -> h1
                  2 -> h2
                  n -> h n
  renderH $ renderHaddock a
renderHaddock (Haddock.DocTable (Haddock.Table headerRows bodyRows)) = do
  mapM_ row headerRows
  stringLine "-----"
  mapM_ row bodyRows
  where
    row (Haddock.TableRow cells) = do
      sequence_ . intersperse (string " | ") . map renderCell $ cells
      newline
    renderCell (Haddock.TableCell _ _ content) =
      renderHaddock content

  -- H.table $ do
  --   H.thead $ do
  --     forM_ headerRows $ \row -> do
  --       H.tr $ do
  --         forM_ (Haddock.tableRowCells row) $ \(Haddock.TableCell colspan rowspan body) -> do
  --           H.th ! HA.colspan (H.toValue colspan)
  --                ! HA.rowspan (H.toValue rowspan)
  --                $ renderHaddock body
  --   H.tbody $ do
  --     forM_ bodyRows $ \row -> do
  --       H.tr $ do
  --         forM_ (Haddock.tableRowCells row) $ \(Haddock.TableCell colspan rowspan body) -> do
  --           H.td ! HA.colspan (H.toValue colspan)
  --                ! HA.rowspan (H.toValue rowspan)
  --                $ renderHaddock body

data TOC a = TOC a [TOC a]
  deriving (Show, Eq, Ord)

renderTOC :: TOC (String, String) -> Build
renderTOC (TOC (label, _href) children) = do
  p $ do
    string label
    forM_ children renderTOC

stateID :: String -> String -> String
stateID protoName stateName = protoName ++ "_state_" ++ stateName

stateTOC :: String -> String -> TOC (String, String)
stateTOC protoName stateName = TOC (stateName, stateID protoName stateName) []

renderMessageRef :: String -> MessageDescription codec -> Build
renderMessageRef toFrom msg = do
  string (messageName msg)
  string " ("
  string toFrom
  string " "
  formatStateRef (messageToState msg)
  string ")"

renderState :: [MessageDescription codec] -> (StateRef, [Description], AgencyID) -> Build
renderState _ (AnyState, _, _) =
  return ()
renderState msgs (State stateName, descriptions, agency) =
  p $ do
    h 3 $ string stateName
    renderDescriptions descriptions
    p $ do
      string "Agency: "
      case agency of
        ClientAgencyID -> string "client"
        ServerAgencyID -> string "server"
        NobodyAgencyID -> string "nobody"
    unless (null messagesFromHere) $ do
      h 4 $ string "Messages from here:"
      ul $ map (renderMessageRef "to") messagesFromHere
    unless (null messagesToHere) $ do
      h 4 $ string "Messages to here:"
      ul $ map (renderMessageRef "from") messagesToHere
  where
    messagesFromHere = filter (matchState stateName . messageFromState) msgs
    messagesToHere = filter (matchState stateName . messageToState) msgs

    matchState :: String -> StateRef -> Bool
    matchState _ AnyState = True
    matchState a (State b) = a == b

formatStateRef :: StateRef -> Build
formatStateRef AnyState =
  string "any state"
formatStateRef (State name) =
  string name

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
              => MessageDescription codec -> Build
renderMessage msg =
  p $ do
    h 3 $ string (messageName msg)
    renderDescriptions (messageDescription msg)
    h 4 $ string "State Transition"
    p $ do
      formatStateRef (messageFromState msg)
      string " -> "
      formatStateRef (messageToState msg)
    unless (null $ messagePayload msg) $ do
      h 4 $ string "Payload"
      ul $ map string (messagePayload msg)
    h 4 $ string "Serialization Format"
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

renderProtocol :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
               => ProtocolDescription codec -> Build
renderProtocol proto = do
  let protoName = protocolName proto
      msgs = protocolMessages proto
  p $ do
    h1 $ string protoName
    string "Version ID: "
    string (protocolIdentifier proto)
    newline
    renderDescriptions (protocolDescription proto)
    p $ do
      h2 $ string "States"
      mconcat <$> mapM (renderState msgs) (protocolStates proto)
    p $ do
      h2 $ string "Messages"
      mconcat <$> mapM renderMessage msgs

fieldSpecToHTML :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                => FieldInfo codec -> Build
fieldSpecToHTML fi = do
  forM_ (fieldSpecAnnotations fi) (p . string)
  renderSubfields (compoundField "" [("", fi)])

fieldSpecAnnotations :: FieldInfo codec -> [String]
fieldSpecAnnotations (AnnField ann fi) =
  ann : fieldSpecAnnotations fi
fieldSpecAnnotations _ = []

renderSubfields :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                => FieldInfo codec -> Build
renderSubfields (AnnField _ fi) =
  renderSubfields fi
renderSubfields (AliasField afi) =
  renderSubfields (aliasFieldTarget afi)
renderSubfields (CompoundField cfi) = do
  ul $ map renderSubfield (compoundFieldSubfields cfi)
renderSubfields _ = return ()

renderFieldType :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                => FieldInfo codec -> Build
renderFieldType (AnnField _ fi) =
  renderFieldType fi
renderFieldType (AliasField fi) = do
  string (aliasFieldName fi)
  newline

  string "This type is an alias for: "
  renderFieldType (aliasFieldTarget fi)
renderFieldType (ListField fi) = do
  string "["
  string (shortFieldType (listElemInfo fi))
  string "] "
  newline

  string "items: "
  string $ formatFieldSize $ listSize fi
  newline

  string "item type: "
  renderFieldType (listElemInfo fi)
  renderSubfields (listElemInfo fi)
renderFieldType (ChoiceField fi) = do
  string "Choice "
  case choiceCondition fi of
    IndexField ref -> string ref
    IndexFlag ref mask -> string ref >> string " & " >> string (printf "0x%04x" mask)
  newline
  ul [ string (show n) >> string " = " >> string (formatFieldSize (fieldSize optInfo)) >>
       string ": " >> renderSubfields optInfo
     | (n :: Int, optInfo) <- zip [0,1..] (choiceFieldAlternatives fi)
     ]
renderFieldType (EnumField fi) = do
  string (enumFieldType fi)
  string " (enum)"
  newline
  ul [ string (show val) >> string " = " >> string name
     | (val, name) <- enumFieldValues fi
     ]
renderFieldType (SumField fi) = do
  string (sumFieldType fi)
  string " (union)"
  newline
  case sumFieldAlternatives fi of
    [(_name, sfi)] ->
      p $ renderFieldType sfi
    sfis ->
      ul [ string name >> string ": " >> renderFieldType sfi
         | (name, sfi) <- sfis
         ]
renderFieldType fi =
  string . fieldType $ fi

renderSubfield :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                 => SubfieldInfo codec -> Build
renderSubfield sfi = do
  string (subfieldName sfi)
  renderFieldType (subfieldInfo sfi)
  string "  (size: "
  string (formatFieldSize (fieldSize (subfieldInfo sfi)))
  string ") "
  renderSubfields (subfieldInfo sfi)

renderProtocolDescriptions :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                           => [ProtocolDescription codec] -> LText.Text
renderProtocolDescriptions protos =
  LText.toLazyText $ execWriter (mapM_ renderProtocol protos)

