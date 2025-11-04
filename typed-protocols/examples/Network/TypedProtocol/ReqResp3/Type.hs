{-# LANGUAGE TypeData                 #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Network.TypedProtocol.ReqResp3.Type where

import Network.TypedProtocol.ReqResp


type data ReqType = ReqInt | ReqFib | ReqStr

type SingReqType :: ReqType -> Type
data SingReqType rt where
    SingReqInt :: SingReqType ReqInt
    SingReqFib :: SingReqType ReqFib
    SingReqStr :: SingReqType ReqStr

deriving instance Show (SingReqType st)

instance RequestTypeI ReqInt where
  requestType = SingReqInt
instance RequestTypeI ReqStr where
  requestType = SingReqStr
instance RequestTypeI ReqFib where
  requestType = SingReqFib

instance Compare ReqType where
  -- Order of all types of ReqType kind
  type instance CompareTypes ReqInt ReqInt = EQ
  type instance CompareTypes ReqInt ReqFib = LT
  type instance CompareTypes ReqInt ReqStr = LT

  type instance CompareTypes ReqFib ReqInt = GT
  type instance CompareTypes ReqFib ReqFib = EQ
  type instance CompareTypes ReqFib ReqStr = LT

  type instance CompareTypes ReqStr ReqInt = GT
  type instance CompareTypes ReqStr ReqFib = GT
  type instance CompareTypes ReqStr ReqStr = EQ


  -- term level comparison following the `CompareTypes` type family
  compareTypes SingReqInt SingReqInt = SingEQ
  compareTypes SingReqInt SingReqFib = SingLT
  compareTypes SingReqInt SingReqStr = SingLT

  compareTypes SingReqFib SingReqInt = SingGT
  compareTypes SingReqFib SingReqFib = SingEQ
  compareTypes SingReqFib SingReqStr = SingLT

  compareTypes SingReqStr SingReqInt = SingGT
  compareTypes SingReqStr SingReqFib = SingGT
  compareTypes SingReqStr SingReqStr = SingEQ

  --
  -- lemmas
  --

  reflexivity SingReqInt = Refl
  reflexivity SingReqFib = Refl
  reflexivity SingReqStr = Refl


  transitivity SingReqInt SingReqFib SingReqStr = Refl
  transitivity SingReqStr a _ = case a of {}
  transitivity _ SingReqStr a = case a of {}


  symmetry SingReqInt SingReqInt = Refl
  symmetry SingReqInt SingReqFib = Refl
  symmetry SingReqInt SingReqStr = Refl

  symmetry SingReqFib SingReqInt = Refl
  symmetry SingReqFib SingReqFib = Refl
  symmetry SingReqFib SingReqStr = Refl

  symmetry SingReqStr SingReqInt = Refl
  symmetry SingReqStr SingReqFib = Refl
  symmetry SingReqStr SingReqStr = Refl

instance RequestResponseProtocol ReqType where

  data Request ReqType st where
    MsgRequestInt :: Request ReqType (Active ReqInt)
    MsgRequestFib :: Request ReqType (Active ReqFib)
    MsgRequestStr :: Request ReqType (Active ReqStr)
    MsgDone       :: Request ReqType Terminal

  data Response ReqType st where
    MsgResponseInt :: Int -> Response ReqType ReqInt
    MsgResponseFib :: Int -> Response ReqType ReqFib
    MsgResponseStr :: String -> Response ReqType ReqStr

  type RequestType = SingReqType
