{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.TypedProtocol.Documentation.TH
( describeProtocol
)
where

import Network.TypedProtocol.Documentation.Types

import Control.Monad
#if MIN_VERSION_template_haskell(2,17,0)
-- This import is only needed when 'getDoc' is available.
import Data.Maybe (maybeToList)
#endif
import Data.Maybe (mapMaybe)
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

  let extractAgency :: InstanceDec -> Maybe Name
      extractAgency (TySynInstD (TySynEqn _ _ (PromotedT agency))) = Just agency
      extractAgency dec = error $ "Unexpected InstanceDec: " ++ show dec

  let extractAgencies :: [InstanceDec] -> [Name]
      extractAgencies = mapMaybe extractAgency

  let extractTheAgency :: [InstanceDec] -> Name
      extractTheAgency inst = case extractAgencies inst of
        [agency] -> agency
        xs -> error $ "Incorrect number of agencies: " ++ show xs

  pstates <- forM (datatypeCons info) $ \conInfo -> do
    let conName = constructorName conInfo
    stateDescription <- getDescription conName

    stateAgencies <- reifyInstances ''StateAgency [ConT conName]
    let agencyName = extractTheAgency stateAgencies
        agencyID = case nameBase agencyName of
          "ServerAgency" -> 'ServerAgencyID
          "ClientAgency" -> 'ClientAgencyID
          "NobodyAgency" -> 'NobodyAgencyID
          x -> error $ "Unknown agency type " ++ x ++ " in state " ++ nameBase conName

    return (conName, stateDescription, agencyID)

  let protocolTy = applyTyArgs (ConT protoTyCon) protoTyArgs

  [DataInstD _ _ _ _ cons _] <- reifyInstances ''Message [protocolTy]

  let messageInfos = map (describeProtocolMessage protoTyCon protoTyArgs codecTyCon codecTyArgs . extractConName) cons

  [| ProtocolDescription
        $(litE (stringL pname))
        protoDescription
        ""
        $(listE
            [ [| ( $(makeState $ ConT conName), stateDescription, $(conE agencyID)) |]
            | (conName, stateDescription, agencyID) <- pstates
            ]
         )
         $(listE messageInfos)
   |]

unearthType :: Type -> Type
#if MIN_VERSION_template_haskell(2,17,0)
unearthType (AppT (AppT MulArrowT _) t) = unearthType t
#endif
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
#if MIN_VERSION_template_haskell(2,17,0)
  haddock <- maybeToList <$> getDoc (DeclDoc name)
#else
  -- 'getDoc' does not exist before template-haskell-2.17.0
  let haddock = []
#endif
  annotations <- reifyAnnotations (AnnLookupName name)
  return $ (Description . (:[]) <$> haddock) ++ annotations

unSigTy :: Type -> Type
unSigTy (SigT t _) = t
unSigTy t@(VarT {}) = t
unSigTy t@(PromotedT {}) = t
unSigTy t = error $ show t

makeState :: Type -> ExpQ
makeState (ConT name) = conE 'State `appE` (litE . stringL $ nameBase name)
makeState (PromotedT name) = conE 'State `appE` (litE . stringL $ nameBase name)
makeState (VarT _) = conE 'AnyState
makeState ty = error . show $ ty -- conE 'AnyState

describeProtocolMessage :: Name -> [Name] -> Name -> [Name] -> Name -> ExpQ
describeProtocolMessage protoTyCon protoTyArgs codecTyCon codecTyArgs msgName = do
  msgInfo <- reifyConstructor msgName
  msgTyInfo <- reifyDatatype msgName
  msgDescription <- getDescription msgName


  let payloads = constructorFields msgInfo
#if MIN_VERSION_template_haskell(2,17,0)
      tyVarName :: TyVarBndr a -> Name
      tyVarName (PlainTV n _) = n
      tyVarName (KindedTV n _ _) = n
#else
      tyVarName :: TyVarBndr -> Name
      tyVarName (PlainTV n) = n
      tyVarName (KindedTV n _) = n
#endif

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
        , messageFromState = $(makeState . unearthType $ fromState)
        , messageToState = $(makeState . unearthType $ toState)
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
