{-# LANGUAGE CPP                   #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Network.TypedProtocol.Stateful.Codec.Properties
  ( prop_codecM
  , prop_codec
  , prop_codec_splitsM
  , prop_codec_splits
  , prop_codecs_compatM
  , prop_codecs_compat
  ) where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Stateful.Codec

import Test.QuickCheck
#if !MIN_VERSION_QuickCheck(2,16,0)
import Test.QuickCheck.Monoids.Compat
#endif


-- | The 'Codec' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_codecM
  :: forall ps failure f m bytes.
     ( Monad m
     , Eq (AnyMessage ps f)
     , Show (AnyMessage ps f)
     , Show failure
     )
  => Codec ps failure f m bytes
  -> AnyMessage ps f
  -> m Property
prop_codecM Codec {encode, decode} a@(AnyMessage f (msg :: Message ps st st')) = do
    r <- decode (stateToken :: StateToken st) f >>= runDecoder [encode f msg]
    case r :: Either failure (SomeMessage st) of
      Right (SomeMessage msg') -> return $ AnyMessage f msg' === a
      Left err                 -> return $ counterexample (show err) False

-- | The 'Codec' round-trip property in a pure monad.
--
prop_codec
  :: forall ps failure f m bytes.
     ( Monad m
     , Eq (AnyMessage ps f)
     , Show (AnyMessage ps f)
     , Show failure
     )
  => (forall a. m a -> a)
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> Property
prop_codec runM codec msg =
    runM (prop_codecM codec msg)


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
  :: forall ps failure f m bytes.
     ( Monad m
     , Eq (AnyMessage ps f)
     , Show (AnyMessage ps f)
     , Show failure
     )
  => (bytes -> [[bytes]])   -- ^ alternative re-chunkings of serialised form
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> m Property
prop_codec_splitsM splits
                   Codec {encode, decode} a@(AnyMessage f (msg :: Message ps st st')) = do
    property . foldMap Every <$> sequence
      [ do r <- decode (stateToken :: StateToken st) f >>= runDecoder bytes'
           case r :: Either failure (SomeMessage st) of
             Right (SomeMessage msg') -> return $ AnyMessage f msg' === a
             Left err                 -> return $ counterexample (show err) False

      | let bytes = encode f msg
      , bytes' <- splits bytes ]


-- | Like @'prop_codec_splitsM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codec_splits
  :: forall ps failure f m bytes.
     ( Monad m
     , Eq (AnyMessage ps f)
     , Show (AnyMessage ps f)
     , Show failure
     )
  => (bytes -> [[bytes]])
  -> (forall a. m a -> a)
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> Property
prop_codec_splits splits runM codec msg =
    runM $ prop_codec_splitsM splits codec msg


-- | Compatibility between two codecs of the same protocol.  Encode a message
-- with one codec and decode it with the other one, then compare if the result
-- is the same as initial message.
--
prop_codecs_compatM
  :: forall ps failure f m bytes.
     ( Monad m
     , Eq (AnyMessage ps f)
     , Show (AnyMessage ps f)
     , forall a. Monoid a => Monoid (m a)
     , Show failure
     )
  => Codec ps failure f m bytes
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> m Property
prop_codecs_compatM codecA codecB
                    a@(AnyMessage f (msg :: Message ps st st')) =
    property
      <$> do r <- decode codecB (stateToken :: StateToken st) f >>= runDecoder [encode codecA f msg]
             case r :: Either failure (SomeMessage st) of
               Right (SomeMessage msg') -> return $ Every $ AnyMessage f msg' === a
               Left err                 -> return $ Every $ counterexample (show err) False
      <> do r <- decode codecA (stateToken :: StateToken st) f >>= runDecoder [encode codecB f msg]
            case r :: Either failure (SomeMessage st) of
              Right (SomeMessage msg') -> return $ Every $ AnyMessage f msg' == a
              Left _                   -> return $ Every False

-- | Like @'prop_codecs_compatM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codecs_compat
  :: forall ps failure f m bytes.
     ( Monad m
     , Eq (AnyMessage ps f)
     , Show (AnyMessage ps f)
     , forall a. Monoid a => Monoid (m a)
     , Show failure
     )
  => (forall a. m a -> a)
  -> Codec ps failure f m bytes
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> Property
prop_codecs_compat run codecA codecB msg =
    run $ prop_codecs_compatM codecA codecB msg
