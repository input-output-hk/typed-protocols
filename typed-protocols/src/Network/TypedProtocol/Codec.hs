{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- @UndecidableInstances@ extension is required for defining @Show@ instance of
-- @'AnyMessage'@ and @'AnyMessage'@.
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Network.TypedProtocol.Codec
  ( -- * Defining and using Codecs
    -- ** Codec type
    CodecF (..)
  , Codec
  , Annotator (..)
  , AnnotatedCodec
  , hoistAnnotation
  , unAnnotateCodec
  , hoistCodec
  , isoCodec
  , mapFailureCodec
    -- ** Incremental decoding
  , DecodeStep (..)
  , runDecoder
  , runDecoderPure
  , hoistDecodeStep
  , isoDecodeStep
  , mapFailureDecodeStep
    -- ** Related types
    -- *** SomeMessage
  , SomeMessage (..)
    -- *** StateToken
  , StateToken
  , StateTokenI (..)
    -- *** ActiveState
  , IsActiveState (..)
  , ActiveState
  , ActiveAgency
  , ActiveAgency' (..)
  , notActiveState
    -- *** PeerRole
  , PeerRole (..)
    -- * CodecFailure
  , CodecFailure (..)
    -- * Testing codec properties
    -- ** Codec
  , AnyMessage (AnyMessage, AnyMessageAndAgency)
  , prop_codecM
  , prop_codec
  , prop_codec_splitsM
  , prop_codec_splits
  , prop_codec_binary_compatM
  , prop_codec_binary_compat
  , prop_codecs_compatM
  , prop_codecs_compat

    -- ** AnnotatedCodec
  , prop_anncodecM
  , prop_anncodec
  , prop_anncodec_splitsM
  , prop_anncodec_splits
  , prop_anncodec_binary_compatM
  , prop_anncodec_binary_compat
  , prop_anncodecs_compatM
  , prop_anncodecs_compat

    -- ** CodecF
  , prop_codecFM
  , prop_codecF
  , prop_codecF_splitsM
  , prop_codecF_splits
  , prop_codecF_binary_compatM
  , prop_codecF_binary_compat
  , prop_codecsF_compatM
  , prop_codecsF_compat

    -- ** SomeState
  , SomeState (..)
  ) where

import Control.Exception (Exception)
import Data.Kind (Type)
import Data.Monoid (All (..))

import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver (SomeMessage (..))


-- | A codec for a 'Protocol' handles the encoding and decoding of typed
-- protocol messages. This is typically used when sending protocol messages
-- over untyped channels. The codec chooses the exact encoding, for example
-- encoding in some text-based syntax, or some choice of binary format.
--
-- The codec is parametrised by:
--
-- * The protocol
-- * the type of decoding failures
-- * the monad in which the decoder runs
-- * a functor which wraps the decoder result, e.g. `SomeMessage` or `Annotator`.
-- * the type of the encoded data (typically strings or bytes, or
--   `AnyMessage` for testing purposes with no codec overhead).
--
-- A codec consists of a message encoder and a decoder.
--
-- The `CodecF` type comes with two useful type aliases:
-- * `Codec`          - which can decode protocol messages
-- * `AnnotatedCodec` - which also has access to bytes which were fed to the
--                      codec when decoding a message.
--
-- `AnnotatedCodec` is useful if one wants to decode data structures and retain
-- their CBOR encoding (`decodeWithByteSpan` from `cborg` can be used for that
-- purpose).
--
-- The encoder is supplied both with the message to encode and the current
-- protocol state (matching the message). The protocol state can be either
-- a client or server state, but for either peer role it is a protocol state
-- in which the peer has agency, since those are the only states where a
-- peer needs to encode a message to be able to send it.
--
-- For example a simple text encoder for the ping\/pong protocol could be:
--
-- > encode :: SingI st
-- >        => Message PingPong st st'
-- >        -> String
-- > encode MsgPing = "ping\n"
-- > encode MsgDone = "done\n"
-- > encode MsgPong = "pong\n"
--
-- The decoder is also given the current protocol state and it is expected to
-- be able to decode /any/ message that is valid in that state, but /only/
-- messages that are valid in that state. Messages that are unexpected for the
-- current state should be treated like any other decoding format error.
--
-- While the current protocol state is known, the state that the message will
-- have the peer transition to is not known. For this reason the decoded
-- message is wrapped in the 'SomeMessage' constructor which hides the \"to\"
-- state.
--
-- The decoder uses an incremental decoding interface 'DecodeStep' so that
-- input can be supplied (e.g. from a Channel) bit by bit. This style of
-- decoder allows but does not require a format with message framing where the
-- decoder input matches exactly with the message boundaries.
--
-- > decode :: forall st m. SingI st
-- >        => StateToken st
-- >        -> m (DecodeStep String String m (SomeMessage st))
-- > decode stok =
-- >   decodeTerminatedFrame '\n' $ \str trailing ->
-- >     case (stok, str) of
-- >       (SingBusy, "pong") ->
-- >            DecodeDone (SomeMessage MsgPong) trailing
-- >       (SingIdle, "ping") ->
-- >            DecodeDone (SomeMessage MsgPing) trailing
-- >       (SingIdle, "done") ->
-- >            DecodeDone (SomeMessage MsgDone) trailing
-- >       _ -> DecodeFail ("unexpected message: " ++ str)
--
-- See "typed-protocols-examples" for the full example.
--
-- Note that the pattern matching on the combination of the message string and
-- the protocol state. This neatly fulfils the requirement that we only return
-- messages that are of the correct type for the given protocol state.
--
-- This toy example format uses newlines @\n@ as a framing format. See
-- 'DecodeStep' for suggestions on how to use it for more realistic formats.
--
data CodecF ps failure m (f :: ps -> Type) bytes = Codec {
       encode :: forall (st :: ps) (st' :: ps).
                 StateTokenI st
              => ActiveState st
              => Message ps st st'
              -> bytes,
       -- ^ encode a message into `bytes`

       decode :: forall (st :: ps).
                 ActiveState st
              => StateToken st
              -- evidence for an active state
              -> m (DecodeStep bytes failure m (f st))
       -- ^ decode a message knowing that the current state is `st`
     }

-- | Change functor in which the codec is running.
--
-- | The type of a standard `Codec` for `typed-protocols`.
--
type Codec ps failure m bytes = CodecF ps failure m SomeMessage bytes

-- | A continuation for a decoder which is fed with whole bytes that were used
-- to parse the message.
--
newtype Annotator bytes st = Annotator { runAnnotator :: bytes -> SomeMessage st }

-- | Codec which has access to bytes received from the network to annotate
-- decoded structure.
--
-- AnnotatedCodec works in two stages.  First it is decoding the structure as
-- bytes are received from the network, like a `Codec` does.  The codec returns
-- a continuation `Annotator` which is fed with all bytes used to parse the
-- message.  It is the driver which is responsible for passing bytes which were
-- fed to the incremental codec.
--
type AnnotatedCodec ps failure m bytes = CodecF ps failure m (Annotator bytes) bytes


-- | Transform annotation.
--
hoistAnnotation :: forall ps failure m f g bytes.
                   Functor m
                => (forall st. f st -> g st)
                -> CodecF ps failure m f bytes
                -> CodecF ps failure m g bytes
hoistAnnotation nat codec@Codec { decode } = codec { decode = decode' }
  where
    decode' :: forall (st :: ps).
               ActiveState st
            => StateToken st
            -> m (DecodeStep bytes failure m (g st))
    decode' tok = fmap nat <$> decode tok


-- | Remove annotation. It is only safe if the `Annotator` treats empty input
-- in a safe way.
--
unAnnotateCodec :: forall ps failure m bytes.
                   (Functor m, Monoid bytes)
                => AnnotatedCodec ps failure m bytes
                -> Codec ps failure m bytes
unAnnotateCodec = hoistAnnotation (($ mempty) . runAnnotator)


hoistCodec
  :: ( Functor n )
  => (forall x . m x -> n x)
  -- ^ a natural transformation
  -> CodecF ps failure m f bytes
  -> CodecF ps failure n f bytes
hoistCodec nat codec = codec
  { decode = fmap (hoistDecodeStep nat) . nat . decode codec
  }

-- | Change bytes of a codec.
--
isoCodec :: Functor m
         => (bytes  -> bytes')
         -- ^ map from 'bytes' to `bytes'`
         -> (bytes' -> bytes)
         -- ^ its inverse
         -> CodecF ps failure m f bytes
         -- ^ codec
         -> CodecF ps failure m f bytes'
isoCodec f finv Codec {encode, decode} = Codec {
      encode = \msg -> f $ encode msg,
      decode = \tok -> isoDecodeStep f finv <$> decode tok
    }

-- | Modify failure type.
--
mapFailureCodec
  :: Functor m
  => (failure -> failure')
  -- ^ a function to apply to failure
  -> CodecF ps failure  m f bytes
  -> CodecF ps failure' m f bytes
mapFailureCodec f Codec {encode, decode} = Codec {
    encode = encode,
    decode = \tok -> mapFailureDecodeStep f <$> decode tok
  }


-- | An incremental decoder with return a value of type @a@.
--
-- This interface is not designed to be used directly for implementing
-- decoders, only for running them. In real applications it is expected to use
-- libraries for text or binary decoding and to implement appropriate wrappers
-- to match up with this incremental decoder interface.
--
-- This style of interface already closely matches that provided by libraries
-- such as @attoparsec@ for text formats, and @binary@, @cereal@ and @cborg@
-- for binary formats.
--
data DecodeStep bytes failure m a =

    -- | The decoder has consumed the available input and needs more
    -- to continue. Provide @'Just'@ if more input is available and
    -- @'Nothing'@ otherwise, and you will get a new @'DecodeStep'@.
    DecodePartial (Maybe bytes -> m (DecodeStep bytes failure m a))

    -- | The decoder has successfully finished. This provides the decoded
    -- result value plus any unused input.
  | DecodeDone a (Maybe bytes)

    -- | The decoder ran into an error. The decoder either used
    -- @'fail'@ or was not provided enough input.
  | DecodeFail failure

deriving instance Functor m => Functor (DecodeStep bytes failure m)

-- | Change bytes of 'DecodeStep'.
--

isoDecodeStep
  :: Functor m
  => (bytes -> bytes')
  -- ^ map from 'bytes' to `bytes'`
  -> (bytes' -> bytes)
  -- its inverse
  -> DecodeStep bytes failure m a
  -> DecodeStep bytes' failure m a
isoDecodeStep f  finv  (DecodePartial g)    = DecodePartial (fmap (isoDecodeStep f finv) . g . fmap finv)
isoDecodeStep f  _finv (DecodeDone a bytes) = DecodeDone a (fmap f bytes)
isoDecodeStep _f _finv (DecodeFail failure) = DecodeFail failure


-- | Change functor in which the codec is running.
--
hoistDecodeStep
  :: ( Functor n )
  => (forall x . m x -> n x)
  -- ^ a natural transformation
  -> DecodeStep bytes failure m a
  -> DecodeStep bytes failure n a
hoistDecodeStep nat step = case step of
  DecodeDone a mb -> DecodeDone a mb
  DecodeFail fail_AvoidNameShadow -> DecodeFail fail_AvoidNameShadow
  DecodePartial k -> DecodePartial (fmap (hoistDecodeStep nat) . nat . k)


-- | Modify failure type.
--
mapFailureDecodeStep
  :: Functor m
  => (failure -> failure')
  -- ^ a function to apply to failure
  -> DecodeStep bytes failure  m a
  -> DecodeStep bytes failure' m a
mapFailureDecodeStep f step = case step of
  DecodeDone a mb    -> DecodeDone a mb
  DecodeFail failure -> DecodeFail (f failure)
  DecodePartial k    -> DecodePartial (fmap (mapFailureDecodeStep f) . k)


-- | Each 'Codec' can use whatever @failure@ type is appropriate. This simple
-- exception type is provided for use by simple codecs (e.g. \"identity\") when
-- nothing more than a 'String' is needed. It is an instance of 'Exception'.
--
data CodecFailure = CodecFailureOutOfInput
                  | CodecFailure String
  deriving (Eq, Show)

-- safe instance with @UndecidableInstances@ in scope
instance Exception CodecFailure


--
-- Running decoders
--

-- | Run a codec incremental decoder 'DecodeStep' against a list of input.
--
-- It ignores any unused trailing data. This is useful for demos, quick
-- experiments and tests.
--
-- See also 'Network.TypedProtocol.Driver.runDecoderWithChannel'
--
runDecoder :: Monad m
           => [bytes]
           -- ^ bytes to be fed into the incremental 'DecodeStep'
           -> DecodeStep bytes failure m a
           -- ^ decoder
           -> m (Either failure a)
runDecoder _      (DecodeDone x _trailing) = return (Right x)
runDecoder _      (DecodeFail failure)     = return (Left failure)
runDecoder []     (DecodePartial k)        = k Nothing  >>= runDecoder []
runDecoder (b:bs) (DecodePartial k)        = k (Just b) >>= runDecoder bs


-- | A variant of 'runDecoder' that is suitable for \"pure\" monads that have
-- a run function. This includes 'ST', using 'Control.Monad.ST.runST'.
--
runDecoderPure :: Monad m
               => (forall b. m b -> b)
               -- ^ run monad 'm' in a pure way, e.g. 'runIdentity'
               -> m (DecodeStep bytes failure m a)
               -> [bytes]
               -- ^ input bytes
               -> Either failure a
runDecoderPure runM decoder bs = runM (runDecoder bs =<< decoder)


--
-- Codec properties
--

-- | Any message for a protocol, with a 'StateTokenI' constraint which gives access to
-- protocol state.
--
-- Used where we don't know statically what the state type is, but need the
-- agency and message to match each other.
--
data AnyMessage ps where
  AnyMessage :: forall ps (st :: ps) (st' :: ps).
                ( StateTokenI st
                , ActiveState st
                )
             => Message ps (st :: ps) (st' :: ps)
             -- ^ 'Message' between some states
             -> AnyMessage ps


-- requires @UndecidableInstances@ and @QuantifiedConstraints@.
instance (forall (st :: ps) (st' :: ps). Show (Message ps st st'))
      => Show (AnyMessage ps) where
  showsPrec d (AnyMessage (msg :: Message ps st st')) =
      showParen (d > app_prec) (showString "AnyMessage " . showsPrec (app_prec + 1) msg)
    where
      app_prec = 10


-- | A convenient pattern synonym which unwrap 'AnyMessage' giving both the
-- singleton for the state and the message.
--
pattern AnyMessageAndAgency :: forall ps. ()
                            => forall (st :: ps) (st' :: ps).
                               (StateTokenI st, ActiveState st)
                            => StateToken st
                            -> Message ps st st'
                            -> AnyMessage ps
pattern AnyMessageAndAgency stateToken msg <- AnyMessage (getAgency -> (msg, stateToken))
  where
    AnyMessageAndAgency _ msg = AnyMessage msg
{-# COMPLETE AnyMessageAndAgency #-}

-- | Internal view pattern for 'AnyMessageAndAgency'
--
getAgency :: StateTokenI st => Message ps st st' -> (Message ps st st', StateToken st)
getAgency msg = (msg, stateToken)


-- | The 'CodecF' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_codecFM
  :: forall ps failure m f bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     )
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -- ^ extract message from the functor
  -> CodecF ps failure m f bytes 
  -- ^ annotated codec
  -> AnyMessage ps
  -- ^ some message
  -> m Bool
prop_codecFM runF Codec {encode, decode} (AnyMessage (msg :: Message ps st st')) = do
    let bytes = encode msg
    r <- decode stateToken >>= runDecoder [bytes]
    return $ case r :: Either failure (f st) of
      Right f -> case runF f bytes of
        SomeMessage msg' ->
          AnyMessage msg' == AnyMessage msg
      Left _ -> False


-- | The 'Codec' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_codecM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     )
  => Codec ps failure m bytes
  -- ^ codec
  -> AnyMessage ps
  -- ^ some message
  -> m Bool
  -- ^ returns 'True' iff round trip returns the exact same message
prop_codecM = prop_codecFM const

-- | The 'Codec' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_anncodecM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     )
  => AnnotatedCodec ps failure m bytes 
  -- ^ annotated codec
  -> AnyMessage ps
  -- ^ some message
  -> m Bool
prop_anncodecM = prop_codecFM runAnnotator


-- | The 'CodecF' round-trip property in a pure monad.
--
prop_codecF
  :: forall ps failure m f bytes.
     (Monad m, Eq (AnyMessage ps))
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> (forall a. m a -> a)
  -> CodecF ps failure m f bytes
  -> AnyMessage ps
  -> Bool
prop_codecF runF runM codec msg = runM (prop_codecFM runF codec msg)

-- | The 'Codec' round-trip property in a pure monad.
--
prop_codec
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps))
  => (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> AnyMessage ps
  -> Bool
prop_codec = prop_codecF const


-- | The 'Codec' round-trip property in a pure monad.
--
prop_anncodec
  :: forall ps failure m bytes.
    (Monad m, Eq (AnyMessage ps))
  => (forall a. m a -> a)
  -> AnnotatedCodec ps failure m bytes
  -> AnyMessage ps
  -> Bool
prop_anncodec = prop_codecF runAnnotator


-- | A more general version of 'prop_codec_splitsM' for 'CodecF'.
--
prop_codecF_splitsM
  :: forall ps failure m f bytes.
     (Monad m, Eq (AnyMessage ps), Monoid bytes)
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> CodecF ps failure m f bytes
  -> AnyMessage ps
  -> m Bool
prop_codecF_splitsM runF splits
                    Codec {encode, decode} (AnyMessage (msg :: Message ps st st')) = do
    and <$> sequence
      [ do r <- decode stateToken >>= runDecoder bytes'
           case r :: Either failure (f st) of
             Right f -> case runF f (mconcat bytes') of
               SomeMessage msg' ->
                 return $! AnyMessage msg' == AnyMessage msg
             Left _                   -> return False

      | let bytes = encode msg
      , bytes' <- splits bytes ]


-- | A variant on the codec round-trip property: given the encoding of a
-- message, check that decode always gives the same result irrespective
-- of how the chunks of input are fed to the incremental decoder.
--
-- This property guards against boundary errors in incremental decoders.
-- It is not necessary to check this for every message type, just for each
-- generic codec construction. For example given some binary serialisation
-- library one would write a generic adaptor to the codec interface. This
-- adaptor has to deal with the incremental decoding and this is what needs
-- to be checked.
--
prop_codec_splitsM
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps), Monoid bytes)
  => (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> Codec ps failure m bytes
  -> AnyMessage ps
  -> m Bool
prop_codec_splitsM = prop_codecF_splitsM const

-- | A variant of 'prop_codec_splitsM' for 'AnnotatedCodec'.
--
prop_anncodec_splitsM
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps), Monoid bytes)
  => (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> AnnotatedCodec ps failure m bytes
  -> AnyMessage ps
  -> m Bool
prop_anncodec_splitsM = prop_codecF_splitsM runAnnotator


-- | A more general version of 'prop_codec_splits' for 'CodecF'.
--
prop_codecF_splits
  :: forall ps failure m f bytes.
     (Monad m, Eq (AnyMessage ps), Monoid bytes)
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> (forall a. m a -> a)
  -> CodecF ps failure m f bytes
  -> AnyMessage ps
  -> Bool
prop_codecF_splits runF splits runM codec msg =
    runM $ prop_codecF_splitsM runF splits codec msg

-- | Like @'prop_codec_splitsM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codec_splits
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps), Monoid bytes)
  => (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> AnyMessage ps
  -> Bool
prop_codec_splits = prop_codecF_splits const

-- | Like 'prop_codec_splits' but for 'AnnotatorCodec'.
prop_anncodec_splits
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps), Monoid bytes)
  => (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> (forall a. m a -> a)
  -> AnnotatedCodec ps failure m bytes
  -> AnyMessage ps
  -> Bool
prop_anncodec_splits = prop_codecF_splits runAnnotator


-- | Auxiliary definition for 'prop_codec_binary_compatM'.
--
-- Used for the existential @st :: ps@ parameter when expressing that for each
-- value of 'PeerHasAgency' for protocol A, there is a corresponding
-- 'PeerHasAgency' for protocol B of some @st :: ps@.
data SomeState (ps :: Type) where
  SomeState
    :: forall ps (st :: ps).
       ActiveState st
    => StateToken st
    -- ^ state token for some active state 'st'
    -> SomeState ps

-- | A more general version of 'prop_codec_binary_compatM' for 'CodecF'.
--
prop_codecF_binary_compatM
  :: forall psA psB failure m fA fB bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     )
  => (forall (st :: psA). fA st -> bytes -> SomeMessage st)
  -> (forall (st :: psB). fB st -> bytes -> SomeMessage st)
  -> CodecF psA failure m fA bytes
  -> CodecF psB failure m fB bytes
  -> (forall (stA :: psA). ActiveState stA => StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> m Bool
prop_codecF_binary_compatM
    runFA runFB codecA codecB stokEq
    (AnyMessage (msgA :: Message psA stA stA')) =
  let stokA :: StateToken stA
      stokA = stateToken
  in case stokEq stokA of
    SomeState (stokB :: StateToken stB) -> do
      -- 1.
      let bytesA = encode codecA msgA
      -- 2.
      r1 <- decode codecB stokB >>= runDecoder [bytesA]
      case r1 :: Either failure (fB stB) of
        Left _     -> return False
        Right fB ->
          case runFB fB bytesA of
            (SomeMessage msgB) -> do
              -- 3.
              let bytesB = encode codecB msgB
              -- 4.
              r2 <- decode codecA (stateToken :: StateToken stA) >>= runDecoder [bytesB]
              case r2 :: Either failure (fA stA) of
                Left _   -> return False
                Right fA ->
                  case runFA fA bytesB of
                    SomeMessage msgA' -> return $ AnyMessage msgA' == AnyMessage msgA

-- | Binary compatibility of two protocols
--
-- We check the following property:
--
-- 1. Using codec A, we encode a message of protocol @psA@ to @bytes@.
--
-- 2. When we decode those @bytes@ using codec B, we get a message of protocol
-- @ps@B.
--
-- 3. When we encode that message again using codec B, we get @bytes@.
--
-- 4. When we decode those @bytes@ using codec A, we get the original message
-- again.
prop_codec_binary_compatM
  :: forall psA psB failure m bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     )
  => Codec psA failure m bytes
  -> Codec psB failure m bytes
  -> (forall (stA :: psA). ActiveState stA => StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> m Bool
prop_codec_binary_compatM = prop_codecF_binary_compatM const const


-- | A version of 'prop_codec_binary_compatM' for 'AnnotatedCodec'.
--
prop_anncodec_binary_compatM
  :: forall psA psB failure m bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     )
  => AnnotatedCodec psA failure m bytes
  -> AnnotatedCodec psB failure m bytes
  -> (forall (stA :: psA). ActiveState stA => StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> m Bool
prop_anncodec_binary_compatM = prop_codecF_binary_compatM runAnnotator runAnnotator


-- | A more general version of 'prop_codec_binary_compat' for 'CodecF'.
--
prop_codecF_binary_compat
  :: forall psA psB failure m fA fB bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     )
  => (forall (st :: psA). fA st -> bytes -> SomeMessage st)
  -> (forall (st :: psB). fB st -> bytes -> SomeMessage st)
  -> (forall a. m a -> a)
  -> CodecF psA failure m fA bytes
  -> CodecF psB failure m fB bytes
  -> (forall (stA :: psA). StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> Bool
prop_codecF_binary_compat runFA runFB runM codecA codecB stokEq msg =
    runM $ prop_codecF_binary_compatM runFA runFB codecA codecB stokEq msg


-- | Like @'prop_codec_splitsM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codec_binary_compat
  :: forall psA psB failure m bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     )
  => (forall a. m a -> a)
  -> Codec psA failure m bytes
  -> Codec psB failure m bytes
  -> (forall (stA :: psA). StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> Bool
prop_codec_binary_compat =
     prop_codecF_binary_compat const const

-- | A 'prop_codec_binary_compat' version for 'AnnotatedCodec'.
--
prop_anncodec_binary_compat
  :: forall psA psB failure m bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     )
  => (forall a. m a -> a)
  -> AnnotatedCodec psA failure m bytes
  -> AnnotatedCodec psB failure m bytes
  -> (forall (stA :: psA). StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> Bool
prop_anncodec_binary_compat runM codecA codecB stokEq msgA =
     runM $ prop_anncodec_binary_compatM codecA codecB stokEq msgA


-- | A more general version of 'prop_codecs_compatM' for 'CodecF'.
--
prop_codecsF_compatM
  :: forall ps failure m f bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> CodecF ps failure m f bytes
  -- ^ first codec
  -> CodecF ps failure m f bytes
  -- ^ second codec
  -> AnyMessage ps
  -- ^ some message
  -> m Bool
prop_codecsF_compatM runF codecA codecB
                    (AnyMessage (msg :: Message ps st st')) =
    
    getAll <$> do let bytes = encode codecA msg
                  r <- decode codecB (stateToken :: StateToken st) >>= runDecoder [bytes]
                  case r :: Either failure (f st) of
                    Right f -> case runF f bytes of
                      SomeMessage msg' -> return $! All $ AnyMessage msg' == AnyMessage msg
                    Left _             -> return $! All False
               
            <> do let bytes = encode codecB msg
                  r <- decode codecA (stateToken :: StateToken st) >>= runDecoder [bytes]
                  case r :: Either failure (f st) of
                    Right f -> case runF f bytes of
                      SomeMessage msg' -> return $! All $ AnyMessage msg' == AnyMessage msg
                    Left _             -> return $! All False

-- | Compatibility between two codecs of the same protocol.  Encode a message
-- with one codec and decode it with the other one, then compare if the result
-- is the same as initial message.
--
prop_codecs_compatM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , forall a. Monoid a => Monoid (m a)
     )
  => Codec ps failure m bytes
  -- ^ first codec
  -> Codec ps failure m bytes
  -- ^ second codec
  -> AnyMessage ps
  -- ^ some message
  -> m Bool
prop_codecs_compatM = prop_codecsF_compatM const

-- | A version of 'prop_codec_compatM' for 'AnnotatedCodec'.
--
prop_anncodecs_compatM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , forall a. Monoid a => Monoid (m a)
     )
  => AnnotatedCodec ps failure m bytes
  -- ^ first codec
  -> AnnotatedCodec ps failure m bytes
  -- ^ second codec
  -> AnyMessage ps
  -- ^ some message
  -> m Bool
prop_anncodecs_compatM = prop_codecsF_compatM runAnnotator


-- | A more general version of 'prop_codecs_compat' for 'CodecF'.
--
prop_codecsF_compat
  :: forall ps failure m f bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> (forall a. m a -> a)
  -> CodecF ps failure m f bytes
  -> CodecF ps failure m f bytes
  -> AnyMessage ps
  -> Bool
prop_codecsF_compat runF runM codecA codecB msg =
    runM $ prop_codecsF_compatM runF codecA codecB msg

-- | Like @'prop_codecs_compatM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codecs_compat
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> Codec ps failure m bytes
  -> AnyMessage ps
  -> Bool
prop_codecs_compat = prop_codecsF_compat const

-- | A version of 'prop_codecs_compat' for 'AnnotatedCodec'.
--
prop_anncodecs_compat
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall a. m a -> a)
  -> AnnotatedCodec ps failure m bytes
  -> AnnotatedCodec ps failure m bytes
  -> AnyMessage ps
  -> Bool
prop_anncodecs_compat = prop_codecsF_compat runAnnotator
