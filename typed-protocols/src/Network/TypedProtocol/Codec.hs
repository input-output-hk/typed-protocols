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
    -- ** SomeState
  , SomeState (..)
  ) where

import Control.Exception (Exception)
import Data.Kind (Type)

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
