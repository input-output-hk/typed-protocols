{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
-- @UndecidableInstances@ extension is required for defining @Show@ instance of
-- @'AnyMessage'@ and @'AnyMessage'@.
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}

-- | Stateful codec.  This module is intended to be imported qualified.
--
module Network.TypedProtocol.Stateful.Codec
  ( -- * Defining and using Codecs
    Codec (..)
  , hoistCodec
  , isoCodec
  , mapFailureCodec
  , liftCodec
    -- ** Incremental decoding
  , DecodeStep (..)
  , runDecoder
  , runDecoderPure
    -- ** Related types
    -- *** SomeMessage
  , SomeMessage (..)
    -- *** StateToken 
  , StateToken
  , StateTokenI (..)
    -- *** ActiveState
  , ActiveState
    -- *** PeerRole
  , PeerRole (..)
    -- * CodecFailure
  , CodecFailure (..)
    -- * Testing codec properties
  , AnyMessage (..)
  , pattern AnyMessageAndAgency
  , showAnyMessage
  , prop_codecM
  , prop_codec
  , prop_codec_splitsM
  , prop_codec_splits
  , prop_codecs_compatM
  , prop_codecs_compat
  ) where

import           Data.Kind (Type)
import           Data.Monoid (All (..))

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec (CodecFailure (..),
                   DecodeStep (..),  SomeMessage (..), hoistDecodeStep,
                   isoDecodeStep, mapFailureDecodeStep, runDecoder,
                   runDecoderPure)
import qualified Network.TypedProtocol.Codec as TP


-- | A stateful codec.
--
data Codec ps failure (f :: ps -> Type) m bytes = Codec {
       encode :: forall (st :: ps) (st' :: ps).
                 StateTokenI st
              => ActiveState st
              => f st
              -> Message ps st st'
              -> bytes,

       decode :: forall (st :: ps).
                 ActiveState st
              => StateToken st
              -> f st
              -> m (DecodeStep bytes failure m (SomeMessage st))
     }

liftCodec :: TP.Codec ps failure m bytes -> Codec ps failure f m bytes
liftCodec codec = Codec { encode = \_ msg -> TP.encode codec msg
                        , decode = \stok _ -> TP.decode codec stok
                        }

hoistCodec
  :: ( Functor n )
  => (forall x . m x -> n x)
  -> Codec ps failure f m bytes
  -> Codec ps failure f n bytes
hoistCodec nat codec = codec
  { decode = \stok f -> fmap (hoistDecodeStep nat) . nat $ decode codec stok f
  }

isoCodec :: Functor m
         => (bytes -> bytes')
         -> (bytes' -> bytes)
         -> Codec ps failure f m bytes
         -> Codec ps failure f m bytes'
isoCodec g finv Codec {encode, decode} = Codec {
      encode = \f msg -> g $ encode f msg,
      decode = \stok f -> isoDecodeStep g finv <$> decode stok f
    }

mapFailureCodec
  :: Functor m
  => (failure -> failure')
  -> Codec ps failure  f m bytes
  -> Codec ps failure' f m bytes
mapFailureCodec g Codec {encode, decode} = Codec {
    encode = encode,
    decode = \stok f -> mapFailureDecodeStep g <$> decode stok f
  }


--
-- Codec properties
--

-- | Any message for a protocol, with a 'StateTokenI' constraint which gives access
-- to protocol state.
--
-- Used where we don't know statically what the state type is, but need the
-- agency and message to match each other.
--
data AnyMessage ps (f :: ps -> Type) where
  AnyMessage :: forall ps f (st :: ps) (st' :: ps).
                ( StateTokenI st
                , ActiveState st
                )
             => f st
             -> Message ps (st :: ps) (st' :: ps)
             -> AnyMessage ps f


-- | `showAnyMessage` is can be used to provide `Show` instance for
-- `AnyMessage` if showing `Message` is independent of the state or one accepts
-- showing only partial information included in message constructors or accepts
-- message constructors to carry `Show` instances for its arguments.  Note that
-- the proper solution is to define a custom `Show (AnyMessage ps f)` instance
-- for a protocol `ps`, which give access to the state functor `f` in scope of
-- `show`.
--
showAnyMessage :: forall ps f.
                  ( forall st st'. Show (Message ps st st')
                  , forall st. Show (f st)
                  )
               => AnyMessage ps f
               -> String
showAnyMessage (AnyMessage st msg) =
    concat [ "AnyMessage "
           , show st
           , " "
           , show msg
           ]


-- | A convenient pattern synonym which unwrap 'AnyMessage' giving both the
-- singleton for the state and the message.
--
pattern AnyMessageAndAgency :: forall ps f. ()
                            => forall (st :: ps) (st' :: ps).
                               (StateTokenI st, ActiveState st)
                            => StateToken st
                            -> f st
                            -> Message ps st st'
                            -> AnyMessage ps f
pattern AnyMessageAndAgency stateToken f msg <- AnyMessage f (getAgency -> (msg, stateToken))
  where
    AnyMessageAndAgency _ msg = AnyMessage msg
{-# COMPLETE AnyMessageAndAgency #-}

-- | Internal view pattern for 'AnyMessageAndAgency'
--
getAgency :: StateTokenI st => Message ps st st' -> (Message ps st st', StateToken st)
getAgency msg = (msg, stateToken)

-- | The 'Codec' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_codecM
  :: forall ps failure f m bytes.
     ( Monad m
     , Eq (AnyMessage ps f)
     )
  => Codec ps failure f m bytes
  -> AnyMessage ps f
  -> m Bool
prop_codecM Codec {encode, decode} a@(AnyMessage f (msg :: Message ps st st')) = do
    r <- decode (stateToken :: StateToken st) f >>= runDecoder [encode f msg]
    case r :: Either failure (SomeMessage st) of
      Right (SomeMessage msg') -> return $ AnyMessage f msg' == a
      Left _                   -> return False

-- | The 'Codec' round-trip property in a pure monad.
--
prop_codec
  :: forall ps failure f m bytes.
     (Monad m, Eq (AnyMessage ps f))
  => (forall a. m a -> a)
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> Bool
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
     (Monad m, Eq (AnyMessage ps f))
  => (bytes -> [[bytes]])   -- ^ alternative re-chunkings of serialised form
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> m Bool
prop_codec_splitsM splits
                   Codec {encode, decode} a@(AnyMessage f (msg :: Message ps st st')) = do
    and <$> sequence
      [ do r <- decode (stateToken :: StateToken st) f >>= runDecoder bytes'
           case r :: Either failure (SomeMessage st) of
             Right (SomeMessage msg') -> return $ AnyMessage f msg' == a
             Left _                   -> return False

      | let bytes = encode f msg
      , bytes' <- splits bytes ]


-- | Like @'prop_codec_splitsM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codec_splits
  :: forall ps failure f m bytes.
     (Monad m, Eq (AnyMessage ps f))
  => (bytes -> [[bytes]])
  -> (forall a. m a -> a)
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> Bool
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
     , forall a. Monoid a => Monoid (m a)
     )
  => Codec ps failure f m bytes
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> m Bool
prop_codecs_compatM codecA codecB
                    a@(AnyMessage f (msg :: Message ps st st')) =
    getAll <$> do r <- decode codecB (stateToken :: StateToken st) f >>= runDecoder [encode codecA f msg]
                  case r :: Either failure (SomeMessage st) of
                    Right (SomeMessage msg') -> return $ All $ AnyMessage f msg' == a
                    Left _                   -> return $ All False
            <> do r <- decode codecA (stateToken :: StateToken st) f >>= runDecoder [encode codecB f msg]
                  case r :: Either failure (SomeMessage st) of
                    Right (SomeMessage msg') -> return $ All $ AnyMessage f msg' == a
                    Left _                   -> return $ All False

-- | Like @'prop_codecs_compatM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codecs_compat
  :: forall ps failure f m bytes.
     ( Monad m
     , Eq (AnyMessage ps f)
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall a. m a -> a)
  -> Codec ps failure f m bytes
  -> Codec ps failure f m bytes
  -> AnyMessage ps f
  -> Bool
prop_codecs_compat run codecA codecB msg =
    run $ prop_codecs_compatM codecA codecB msg
