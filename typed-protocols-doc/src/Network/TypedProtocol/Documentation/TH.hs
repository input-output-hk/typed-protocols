{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.TypedProtocol.Documentation.TH
( describeProtocol
)
where

import Network.TypedProtocol.Documentation.Types

import Control.Monad
import Data.Maybe
import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Network.TypedProtocol.Core


applyTyArgs :: Type -> [Name] -> Type
applyTyArgs t [] = t
applyTyArgs t (x:xs) =
  applyTyArgs (AppT t (ConT x)) xs

-- | Generate a 'ProtocolDescription' runtime representation of a typed
-- protocol specification, including serialization information.
describeProtocol :: Name -> [Name] -> Name -> [Name] -> ExpQ
describeProtocol protoTyCon protoTyArgs codecTyCon codecTyArgs = do
  info <- reifyDatatype protoTyCon
  protoDescription <- getDescription protoTyCon
  let pname = nameBase (datatypeName info)

  let extractMessageStateName :: InstanceDec -> Name
      extractMessageStateName (DataInstD _ _ _ _ [ty] _) =
        case ty of
          GadtC _ _ ty' -> go ty'
          _ -> error $ show ty
        where
          go (PromotedT tyName) = tyName
          go (SigT ty' _) = go ty'
          go (AppT _ ty') = go ty'
          go ty' = error $ show ty'
      extractMessageStateName i = error $ show i

  pstates <- forM (datatypeCons info) $ \conInfo -> do
    let conName = constructorName conInfo
    stateDescription <- getDescription conName

    serverAgencies <- reifyInstances ''ServerHasAgency [ConT conName]
    let serverAgencies' =
          [ nameBase tyName
          | inst <- serverAgencies
          , let tyName = extractMessageStateName inst
          , nameBase tyName == nameBase conName
          ]
    clientAgencies <- reifyInstances ''ClientHasAgency [ConT conName]
    let clientAgencies' =
          [ nameBase tyName
          | inst <- clientAgencies
          , let tyName = extractMessageStateName inst
          , nameBase tyName == nameBase conName
          ]

    let agencyID = case (serverAgencies', clientAgencies') of
          ([], []) -> NobodyAgencyID
          (_, []) -> ServerAgencyID
          ([], _) -> ClientAgencyID
          _ -> error $ show (conName, serverAgencies', clientAgencies')

    return (conName, stateDescription, agencyID)

  let protocolTy = applyTyArgs (ConT protoTyCon) protoTyArgs

  [DataInstD _ _ _ _ cons _] <- reifyInstances ''Message [protocolTy]

  let messageInfos = map (describeProtocolMessage protoTyCon protoTyArgs codecTyCon codecTyArgs . extractConName) cons

  [| ProtocolDescription
        $(litE (stringL pname))
        protoDescription
        ""
        $(listE
            [ [| ( $(litE . stringL . nameBase $ conName), stateDescription, agencyID) |]
            | (conName, stateDescription, agencyID) <- pstates
            ]
         )
         $(listE messageInfos)
   |]

unearthType :: Type -> Type
unearthType (AppT (AppT MulArrowT _) t) = unearthType t
-- unearthType (AppT a _) = unearthType a
unearthType (SigT a _) = unearthType a
unearthType t = t


prettyTy :: Type -> String
prettyTy = snd . go
  where
    go (ConT n) = (False, nameBase n)
    go (PromotedT n) = (False, nameBase n)
    go (VarT n) = (False, nameBase n)
    go (AppT a b) =
      let
        (_, a') = go a
        (wrap, b') = go b
      in
        (True, a' ++ " " ++ if wrap then "(" ++ b' ++ ")" else b')
    go (ForallT _ _ a) = go a
    go (ForallVisT _ a) = go a
    go (AppKindT _ a) = go a
    go t = (True, show t)

getDescription :: Name -> Q [Description]
getDescription name = do
  haddock <- maybeToList <$> getDoc (DeclDoc name)
  annotations <- reifyAnnotations (AnnLookupName name)
  return $ (Description . (:[]) <$> haddock) ++ annotations

unSigTy :: Type -> Type
unSigTy (SigT t _) = t
unSigTy t@(VarT {}) = t
unSigTy t@(PromotedT {}) = t
unSigTy t = error $ show t

describeProtocolMessage :: Name -> [Name] -> Name -> [Name] -> Name -> ExpQ
describeProtocolMessage protoTyCon protoTyArgs codecTyCon codecTyArgs msgName = do
  msgInfo <- reifyConstructor msgName
  msgTyInfo <- reifyDatatype msgName
  msgDescription <- getDescription msgName


  let payloads = constructorFields msgInfo
      tyVarName :: TyVarBndr a -> Name
      tyVarName (PlainTV n _) = n
      tyVarName (KindedTV n _ _) = n

      findType :: Name -> Cxt -> Type
      findType n (AppT (AppT EqualityT (VarT vn)) t : _)
        | vn == n
        = t
      findType n (_ : xs) = findType n xs
      findType n [] = VarT n

      fromStateVar = tyVarName . last . init $ datatypeVars msgTyInfo
      toStateVar = tyVarName . last $ datatypeVars msgTyInfo
      fromState = findType fromStateVar (constructorContext msgInfo)
      toState = findType toStateVar (constructorContext msgInfo)

  [e| MessageDescription
        { messageName = $(litE . stringL . nameBase $ msgName)
        , messageDescription = msgDescription
        , messagePayload = $(listE (map (litE . stringL . prettyTy) payloads))
        , messageFromState = $(litE (stringL . prettyTy $ unearthType fromState))
        , messageToState = $(litE (stringL . prettyTy $ unearthType toState))
        , messageInfo =
            infoOf $(litE . stringL . nameBase $ msgName) $
            info 
              (Proxy :: Proxy $(pure $ applyTyArgs (ConT codecTyCon) codecTyArgs))
              (Proxy :: Proxy ( $(conT $ datatypeName msgTyInfo)
                                   $(pure $ applyTyArgs (ConT protoTyCon) protoTyArgs)
                                   $(pure $ unSigTy fromState)
                                   $(pure $ unSigTy toState)
                                 )
              )
        }
    |]

extractConName :: Con -> Name
extractConName con = case con of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c -> extractConName c
  GadtC (name:_) _ _ -> name
  RecGadtC (name:_) _ _ -> name
  x -> error $ "Cannot extract constructor name from " ++ show x
