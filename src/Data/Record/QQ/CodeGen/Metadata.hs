{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

-- | Deal with the type-level metadata
module Data.Record.QQ.CodeGen.Metadata (
    -- * Parsing
    getTypeLevelMetadata
  ) where

import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Language.Haskell.TH

import qualified Control.Monad.Except as Except

import Data.Record.Generic
import Data.Record.TH.CodeGen.Name
import Data.Record.TH.CodeGen.TH

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | Parse previously constructed type level data
--
-- We do this when we construct record /values/, at which point we have no
-- 'Options', so this must work without options.
--
-- 'Nothing' if this wasn't a type created using @large-records@.
getTypeLevelMetadata ::
     ConstrName
  -> ExceptT String Q (TypeName, ([TyVarBndr], [(FieldName, Type)]))
getTypeLevelMetadata constr = do
    parent    <- reifyConstr constr >>= getDataConParent
    saturated <- Except.lift (reify parent) >>= getSaturatedType
    parsed    <- Except.lift (getMetadataInstance saturated) >>= parseTySynInst
    return (TypeName (nameBase parent), parsed)
  where
    reifyConstr :: ConstrName -> ExceptT String Q Info
    reifyConstr (ConstrName c) = do
        mName <- Except.lift $ lookupValueName c
        case mName of
          Nothing -> throwError $ show c ++ " not in scope"
          Just nm -> Except.lift $ reify nm

    saturate :: Name -> [TyVarBndr] -> Type
    saturate n = foldl (\t v -> t `AppT` VarT (tyVarName v)) (ConT n)

    getMetadataInstance :: Type -> Q [InstanceDec]
    getMetadataInstance = reifyInstances ''MetadataOf . (:[])

    getSaturatedType :: Info -> ExceptT String Q Type
    getSaturatedType (TyConI (NewtypeD [] nm tyVars _kind _con _deriv)) =
        return $ saturate nm tyVars
    getSaturatedType i =
        unexpected i "newtype"

    getDataConParent :: Info -> ExceptT String Q Name
    getDataConParent (DataConI _ _ parent) =
        return parent
    getDataConParent i =
        unexpected i "data constructor"

    parseTySynInst ::
         [InstanceDec]
      -> ExceptT String Q ([TyVarBndr], [(FieldName, Type)])
    parseTySynInst [TySynInstD (TySynEqn vars _lhs rhs)] =
        (fromMaybe [] vars, ) <$> parseList rhs
    parseTySynInst is =
        unexpected is "type instance"

    parseList :: Type -> ExceptT String Q [(FieldName, Type)]
    parseList (AppT (AppT PromotedConsT t) ts) =
        (:) <$> parseTuple t <*> parseList ts
    parseList PromotedNilT =
        return []
    parseList (SigT t _kind) =
        parseList t
    parseList t = unexpected t "list"

    parseTuple :: Type -> ExceptT String Q (FieldName, Type)
    parseTuple (AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit f))) t) =
        return (FieldName f, t)
    parseTuple t = unexpected t "tuple"

    unexpected :: Show a => a -> String -> ExceptT String Q b
    unexpected actual expected = throwError $ concat [
          "Unexpected "
        , show actual
        , " (expected "
        , expected
        , ")"
        ]
