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
import Control.Monad.RWS
import Data.List (intersperse)

import Network.TypedProtocol.Documentation.Types

import Data.SerDoc.Class
import Data.SerDoc.Info

type Build = RWS Int LText.Builder Bool ()

runBuild :: Build -> LText.Text
runBuild a = LText.toLazyText b
  where
    ((), _, b) = runRWS a 0 False

indent :: Build
indent = do
  atStart <- get
  when atStart $ do
    put False
    lvl <- ask
    replicateM_ lvl (tell " ")

assertLineStart :: Build
assertLineStart = do
  atStart <- get
  unless atStart newline

withIndent :: Int -> Build -> Build
withIndent i = local (+ i)

write :: LText.Builder -> Build
write b = indent >> tell b

string :: String -> Build
string = write . LText.fromString

newline :: Build
newline = do
  string "\n"
  put True

buildBare :: Build -> LText.Text
buildBare =
  LText.unwords . LText.words . runBuild

p :: Build -> Build
p b = b >> newline

stringLine :: String -> Build
stringLine = p . string

h :: Int -> String -> Build
h 1 s = do
  assertLineStart
  string s
  newline
  stringLine $ replicate (length s) '='
h 2 s = do
  assertLineStart
  string s
  newline
  stringLine $ replicate (length s) '-'
  newline
h n s = do
  assertLineStart
  replicateM_ n $ string "#"
  string " "
  string s
  newline

ul :: [Build] -> Build
ul items = do
  forM_ items $ \item -> do
    assertLineStart
    string "- "
    withIndent 2 item
  assertLineStart

ol :: [(Int, Build)] -> Build
ol items = do
  mapM_ (uncurry renderItem) items
  assertLineStart
  where
    renderItem :: Int -> Build -> Build
    renderItem n item = do
      assertLineStart
      string . show $ n
      string ". "
      withIndent 2 item

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
                  1 -> h 1
                  2 -> h 2
                  n -> h n
  renderH . LText.unpack . buildBare $ renderHaddock a
renderHaddock (Haddock.DocTable (Haddock.Table headerRows bodyRows)) = do
  mapM_ row headerRows
  stringLine "-----"
  mapM_ row bodyRows
  where
    row (Haddock.TableRow cells) = do
      assertLineStart
      sequence_ . intersperse (string " | ") . map renderCell $ cells
      assertLineStart
    renderCell (Haddock.TableCell _ _ content) =
      renderHaddock content

data TOC a = TOC a [TOC a]
  deriving (Show, Eq, Ord)

renderTOC :: TOC (String, String) -> Build
renderTOC (TOC (label, _href) children) = do
  stringLine label
  withIndent 2 $ forM_ children renderTOC

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
    h 3 stateName
    newline
    renderDescriptions descriptions
    p $ do
      string "Agency: "
      case agency of
        ClientAgencyID -> string "client"
        ServerAgencyID -> string "server"
        NobodyAgencyID -> string "nobody"
    unless (null messagesFromHere) $ do
      h 4 "Messages from here:"
      ul $ map (renderMessageRef "to") messagesFromHere
    unless (null messagesToHere) $ do
      h 4 "Messages to here:"
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
    h 3 $ messageName msg
    newline
    renderDescriptions (messageDescription msg)
    h 4 "State Transition"
    p $ do
      formatStateRef (messageFromState msg)
      string " -> "
      formatStateRef (messageToState msg)
    unless (null $ messagePayload msg) $ do
      h 4 "Payload"
      ul $ map string (messagePayload msg)
    h 4 "Serialization Format"
    renderFieldSpec (messageInfo msg)
    newline
  
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
    h 1 protoName
    string (protocolIdentifier proto)
    assertLineStart
    renderDescriptions (protocolDescription proto)
    p $ do
      h 2 "States"
      mconcat <$> mapM (renderState msgs) (protocolStates proto)
    p $ do
      h 2 "Messages"
      mconcat <$> mapM renderMessage msgs

renderFieldSpec :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                => FieldInfo codec -> Build
renderFieldSpec fi = do
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
  assertLineStart

  string "This type is an alias for: "
  renderFieldType (aliasFieldTarget fi)
renderFieldType (ListField fi) = do
  string "["
  string (shortFieldType (listElemInfo fi))
  string "] "
  assertLineStart

  string "items: "
  string $ formatFieldSize $ listSize fi
  assertLineStart

  string "item type: "
  renderFieldType (listElemInfo fi)
  renderSubfields (listElemInfo fi)
renderFieldType (ChoiceField fi) = do
  string "Choice ("
  case choiceCondition fi of
    IndexField ref -> string ref
    IndexFlag ref mask -> string ref >> string " & " >> string (printf "0x%04x" mask)
  string ")"
  assertLineStart
  ul [ string (show n) >>
       string ": " >> renderSubfields optInfo >>
       assertLineStart >>
       string "size: " >> string (formatFieldSize (fieldSize optInfo)) >>
       assertLineStart
     | (n :: Int, optInfo) <- zip [0,1..] (choiceFieldAlternatives fi)
     ]
renderFieldType (EnumField fi) = do
  string (enumFieldType fi)
  string " (enum)"
  assertLineStart
  ul [ string (show val) >> string " = " >> string name
     | (val, name) <- enumFieldValues fi
     ]
renderFieldType (SumField fi) = do
  string (sumFieldType fi)
  string " (union)"
  assertLineStart
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
  assertLineStart
  renderFieldType (subfieldInfo sfi)
  assertLineStart
  string "size: "
  string (formatFieldSize (fieldSize (subfieldInfo sfi)))
  string " "
  renderSubfields (subfieldInfo sfi)

renderProtocolDescriptions :: (HasInfo codec Word32, HasInfo codec (DefEnumEncoding codec))
                           => [ProtocolDescription codec] -> LText.Text
renderProtocolDescriptions protos = runBuild $ do
    h 1 "Table Of Contents"
    p $ renderTOC (TOC ("Protocols","") (map protocolTOC protos))
    mapM_ renderProtocol protos
