{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeData           #-}

module Data.Type.NatMap.Types
  ( type KeyVal (..)
  , type NatMap (..)
  ) where

import Network.TypedProtocol.Core (N (..))


type data KeyVal ps = ps :-> N
infixl 6 :->


-- | Type level map from states of kind `ps` to type level natural numbers.
--
type data NatMap ps where
  Cons  :: KeyVal ps
        -> NatMap ps
        -> NatMap ps
  Empty :: NatMap ps

