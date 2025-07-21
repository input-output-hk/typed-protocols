{-# LANGUAGE QuantifiedConstraints #-}

module Network.TypedProtocol.Codec.Properties
  ( -- * Codec Properties
    prop_codecM
  , prop_codec
  , prop_codec_splitsM
  , prop_codec_splits
  , prop_codec_binary_compatM
  , prop_codec_binary_compat
  , prop_codecs_compatM
  , prop_codecs_compat
    -- ** AnnotatedCodec Properties
  , prop_anncodecM
  , prop_anncodec
  , prop_anncodec_splitsM
  , prop_anncodec_splits
  , prop_anncodec_binary_compatM
  , prop_anncodec_binary_compat
  , prop_anncodecs_compatM
  , prop_anncodecs_compat
    -- ** CodecF Properties
  , prop_codecFM
  , prop_codecF
  , prop_codecF_splitsM
  , prop_codecF_splits
  , prop_codecF_binary_compatM
  , prop_codecF_binary_compat
  , prop_codecsF_compatM
  , prop_codecsF_compat
    -- * Re-exports
  , AnyMessage (..)
  , SomeState (..)
  ) where

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core

import Test.QuickCheck


-- | The 'CodecF' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_codecFM
  :: forall ps failure m f bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     )
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -- ^ extract message from the functor
  -> CodecF ps failure m f bytes
  -- ^ annotated codec
  -> AnyMessage ps
  -- ^ some message
  -> m Property
prop_codecFM runF Codec {encode, decode} (AnyMessage (msg :: Message ps st st')) = do
    let bytes = encode msg
    r <- decode stateToken >>= runDecoder [bytes]
    return $ case r :: Either failure (f st) of
      Right f -> case runF f bytes of
        SomeMessage msg' ->
          AnyMessage msg' === AnyMessage msg
      Left err -> counterexample (show err) False


-- | The 'Codec' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_codecM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     )
  => Codec ps failure m bytes
  -- ^ codec
  -> AnyMessage ps
  -- ^ some message
  -> m Property
  -- ^ returns 'True' iff round trip returns the exact same message
prop_codecM = prop_codecFM const

-- | The 'Codec' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_anncodecM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     )
  => AnnotatedCodec ps failure m bytes
  -- ^ annotated codec
  -> AnyMessage ps
  -- ^ some message
  -> m Property
prop_anncodecM = prop_codecFM runAnnotator


-- | The 'CodecF' round-trip property in a pure monad.
--
prop_codecF
  :: forall ps failure m f bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     )
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> (forall a. m a -> a)
  -> CodecF ps failure m f bytes
  -> AnyMessage ps
  -> Property
prop_codecF runF runM codec msg = runM (prop_codecFM runF codec msg)

-- | The 'Codec' round-trip property in a pure monad.
--
prop_codec
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     )
  => (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> AnyMessage ps
  -> Property
prop_codec = prop_codecF const


-- | The 'Codec' round-trip property in a pure monad.
--
prop_anncodec
  :: forall ps failure m bytes.
    ( Monad m
    , Eq (AnyMessage ps)
    , Show (AnyMessage ps)
    , Show failure
    )
  => (forall a. m a -> a)
  -> AnnotatedCodec ps failure m bytes
  -> AnyMessage ps
  -> Property
prop_anncodec = prop_codecF runAnnotator


-- | A more general version of 'prop_codec_splitsM' for 'CodecF'.
--
prop_codecF_splitsM
  :: forall ps failure m f bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , Monoid bytes
     )
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> CodecF ps failure m f bytes
  -> AnyMessage ps
  -> m Property
prop_codecF_splitsM runF splits
                    Codec {encode, decode} (AnyMessage (msg :: Message ps st st')) = do
    property . foldMap Every <$> sequence
      [ do r <- decode stateToken >>= runDecoder bytes'
           case r :: Either failure (f st) of
             Right f -> case runF f (mconcat bytes') of
               SomeMessage msg' ->
                 return $! AnyMessage msg' === AnyMessage msg
             Left err -> return $ counterexample (show err) False

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
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , Monoid bytes
     )
  => (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> Codec ps failure m bytes
  -> AnyMessage ps
  -> m Property
prop_codec_splitsM = prop_codecF_splitsM const

-- | A variant of 'prop_codec_splitsM' for 'AnnotatedCodec'.
--
prop_anncodec_splitsM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , Monoid bytes
     )
  => (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> AnnotatedCodec ps failure m bytes
  -> AnyMessage ps
  -> m Property
prop_anncodec_splitsM = prop_codecF_splitsM runAnnotator


-- | A more general version of 'prop_codec_splits' for 'CodecF'.
--
prop_codecF_splits
  :: forall ps failure m f bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , Monoid bytes
     )
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> (forall a. m a -> a)
  -> CodecF ps failure m f bytes
  -> AnyMessage ps
  -> Property
prop_codecF_splits runF splits runM codec msg =
    runM $ prop_codecF_splitsM runF splits codec msg

-- | Like @'prop_codec_splitsM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codec_splits
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , Monoid bytes
     )
  => (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> AnyMessage ps
  -> Property
prop_codec_splits = prop_codecF_splits const

-- | Like 'prop_codec_splits' but for 'AnnotatorCodec'.
prop_anncodec_splits
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , Monoid bytes
     )
  => (bytes -> [[bytes]])
  -- ^ alternative re-chunkings of serialised form
  -> (forall a. m a -> a)
  -> AnnotatedCodec ps failure m bytes
  -> AnyMessage ps
  -> Property
prop_anncodec_splits = prop_codecF_splits runAnnotator

-- | A more general version of 'prop_codec_binary_compatM' for 'CodecF'.
--
prop_codecF_binary_compatM
  :: forall psA psB failure m fA fB bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     , Show (AnyMessage psA)
     , Show failure
     )
  => (forall (st :: psA). fA st -> bytes -> SomeMessage st)
  -> (forall (st :: psB). fB st -> bytes -> SomeMessage st)
  -> CodecF psA failure m fA bytes
  -> CodecF psB failure m fB bytes
  -> (forall (stA :: psA). ActiveState stA => StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> m Property
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
        Left err   -> return $ counterexample (show err) False
        Right fB ->
          case runFB fB bytesA of
            (SomeMessage msgB) -> do
              -- 3.
              let bytesB = encode codecB msgB
              -- 4.
              r2 <- decode codecA (stateToken :: StateToken stA) >>= runDecoder [bytesB]
              case r2 :: Either failure (fA stA) of
                Left err -> return $ counterexample (show err) False
                Right fA ->
                  case runFA fA bytesB of
                    SomeMessage msgA' -> return $ AnyMessage msgA' === AnyMessage msgA

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
     , Show (AnyMessage psA)
     , Show failure
     )
  => Codec psA failure m bytes
  -> Codec psB failure m bytes
  -> (forall (stA :: psA). ActiveState stA => StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> m Property
prop_codec_binary_compatM = prop_codecF_binary_compatM const const


-- | A version of 'prop_codec_binary_compatM' for 'AnnotatedCodec'.
--
prop_anncodec_binary_compatM
  :: forall psA psB failure m bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     , Show (AnyMessage psA)
     , Show failure
     )
  => AnnotatedCodec psA failure m bytes
  -> AnnotatedCodec psB failure m bytes
  -> (forall (stA :: psA). ActiveState stA => StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> m Property
prop_anncodec_binary_compatM = prop_codecF_binary_compatM runAnnotator runAnnotator


-- | A more general version of 'prop_codec_binary_compat' for 'CodecF'.
--
prop_codecF_binary_compat
  :: forall psA psB failure m fA fB bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     , Show (AnyMessage psA)
     , Show failure
     )
  => (forall (st :: psA). fA st -> bytes -> SomeMessage st)
  -> (forall (st :: psB). fB st -> bytes -> SomeMessage st)
  -> (forall a. m a -> a)
  -> CodecF psA failure m fA bytes
  -> CodecF psB failure m fB bytes
  -> (forall (stA :: psA). StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> Property
prop_codecF_binary_compat runFA runFB runM codecA codecB stokEq msg =
    runM $ prop_codecF_binary_compatM runFA runFB codecA codecB stokEq msg


-- | Like @'prop_codec_splitsM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codec_binary_compat
  :: forall psA psB failure m bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     , Show (AnyMessage psA)
     , Show failure
     )
  => (forall a. m a -> a)
  -> Codec psA failure m bytes
  -> Codec psB failure m bytes
  -> (forall (stA :: psA). StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> Property
prop_codec_binary_compat =
     prop_codecF_binary_compat const const

-- | A 'prop_codec_binary_compat' version for 'AnnotatedCodec'.
--
prop_anncodec_binary_compat
  :: forall psA psB failure m bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     , Show (AnyMessage psA)
     , Show failure
     )
  => (forall a. m a -> a)
  -> AnnotatedCodec psA failure m bytes
  -> AnnotatedCodec psB failure m bytes
  -> (forall (stA :: psA). StateToken stA -> SomeState psB)
     -- ^ the states of A map directly to states of B.
  -> AnyMessage psA
  -> Property
prop_anncodec_binary_compat runM codecA codecB stokEq msgA =
     runM $ prop_anncodec_binary_compatM codecA codecB stokEq msgA


-- | A more general version of 'prop_codecs_compatM' for 'CodecF'.
--
prop_codecsF_compatM
  :: forall ps failure m f bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> CodecF ps failure m f bytes
  -- ^ first codec
  -> CodecF ps failure m f bytes
  -- ^ second codec
  -> AnyMessage ps
  -- ^ some message
  -> m Property
prop_codecsF_compatM runF codecA codecB
                    (AnyMessage (msg :: Message ps st st')) =

    property
           <$> do let bytes = encode codecA msg
                  r <- decode codecB (stateToken :: StateToken st) >>= runDecoder [bytes]
                  case r :: Either failure (f st) of
                    Right f -> case runF f bytes of
                      SomeMessage msg' -> return $! Every $ AnyMessage msg' === AnyMessage msg
                    Left err           -> return $! Every $ counterexample (show err) False

           <>  do let bytes = encode codecB msg
                  r <- decode codecA (stateToken :: StateToken st) >>= runDecoder [bytes]
                  case r :: Either failure (f st) of
                    Right f -> case runF f bytes of
                      SomeMessage msg' -> return $! Every $ AnyMessage msg' === AnyMessage msg
                    Left err           -> return $! Every $ counterexample (show err) False

-- | Compatibility between two codecs of the same protocol.  Encode a message
-- with one codec and decode it with the other one, then compare if the result
-- is the same as initial message.
--
prop_codecs_compatM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , forall a. Monoid a => Monoid (m a)
     )
  => Codec ps failure m bytes
  -- ^ first codec
  -> Codec ps failure m bytes
  -- ^ second codec
  -> AnyMessage ps
  -- ^ some message
  -> m Property
prop_codecs_compatM = prop_codecsF_compatM const

-- | A version of 'prop_codec_compatM' for 'AnnotatedCodec'.
--
prop_anncodecs_compatM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , forall a. Monoid a => Monoid (m a)
     )
  => AnnotatedCodec ps failure m bytes
  -- ^ first codec
  -> AnnotatedCodec ps failure m bytes
  -- ^ second codec
  -> AnyMessage ps
  -- ^ some message
  -> m Property
prop_anncodecs_compatM = prop_codecsF_compatM runAnnotator


-- | A more general version of 'prop_codecs_compat' for 'CodecF'.
--
prop_codecsF_compat
  :: forall ps failure m f bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall (st :: ps). f st -> bytes -> SomeMessage st)
  -> (forall a. m a -> a)
  -> CodecF ps failure m f bytes
  -> CodecF ps failure m f bytes
  -> AnyMessage ps
  -> Property
prop_codecsF_compat runF runM codecA codecB msg =
    runM $ prop_codecsF_compatM runF codecA codecB msg

-- | Like @'prop_codecs_compatM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codecs_compat
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> Codec ps failure m bytes
  -> AnyMessage ps
  -> Property
prop_codecs_compat = prop_codecsF_compat const

-- | A version of 'prop_codecs_compat' for 'AnnotatedCodec'.
--
prop_anncodecs_compat
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , Show (AnyMessage ps)
     , Show failure
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall a. m a -> a)
  -> AnnotatedCodec ps failure m bytes
  -> AnnotatedCodec ps failure m bytes
  -> AnyMessage ps
  -> Property
prop_anncodecs_compat = prop_codecsF_compat runAnnotator
