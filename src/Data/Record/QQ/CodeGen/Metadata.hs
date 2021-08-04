{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

-- | Deal with the type-level metadata
module Data.Record.QQ.CodeGen.Metadata (
    -- * Parsing
    getTypeLevelMetadata
  ) where

import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Control.Monad.Except as Except

import Data.Record.Generic hiding (FieldName)
import Data.Record.TH.CodeGen.TH
import Data.Record.TH.CodeGen.Name (TypeName, ConstrName, FieldName)

import qualified Data.Record.TH.CodeGen.Name as N

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | Parse previously constructed type level data
--
-- We do this when we construct record /values/, at which point we have no
-- 'Options', so this must work without options.
--
-- 'Nothing' if this wasn't a type created using @large-records@.
getTypeLevelMetadata :: forall m.
     Quasi m
  => ConstrName 'N.Global
  -> ExceptT String m (TypeName 'N.Global, ([TyVarBndr], [(FieldName, Type)]))
getTypeLevelMetadata constr = do
    parent    <- Except.lift (N.reify constr) >>= getDataConParent
    saturated <- Except.lift (N.reify parent) >>= getSaturatedType
    parsed    <- Except.lift (getMetadataInstance saturated) >>= parseTySynInst
    return (parent, parsed)
  where
    saturate :: Name -> [TyVarBndr] -> Type
    saturate n = foldl (\t v -> t `AppT` VarT (tyVarName v)) (ConT n)

    getMetadataInstance :: Type -> m [InstanceDec]
    getMetadataInstance = runQ . reifyInstances ''MetadataOf . (:[])

    getSaturatedType :: Info -> ExceptT String m Type
    getSaturatedType (TyConI (NewtypeD [] nm tyVars _kind _con _deriv)) =
        return $ saturate nm tyVars
    getSaturatedType i =
        unexpected i "newtype"

    getDataConParent :: Info -> ExceptT String m (TypeName 'N.Global)
    getDataConParent (DataConI _ _ parent) =
        return $ N.TypeName $ N.fromName' parent
    getDataConParent i =
        unexpected i "data constructor"

    parseTySynInst ::
         [InstanceDec]
      -> ExceptT String m ([TyVarBndr], [(FieldName, Type)])
    parseTySynInst [TySynInstD (TySynEqn vars _lhs rhs)] =
        (fromMaybe [] vars, ) <$> parseList rhs
    parseTySynInst is =
        unexpected is "type instance"

    parseList :: Type -> ExceptT String m [(FieldName, Type)]
    parseList (AppT (AppT PromotedConsT t) ts) =
        (:) <$> parseTuple t <*> parseList ts
    parseList PromotedNilT =
        return []
    parseList (SigT t _kind) =
        parseList t
    parseList t = unexpected t "list"

    parseTuple :: Type -> ExceptT String m (FieldName, Type)
    parseTuple (AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit f))) t) =
        return (N.FieldName (N.OverloadedName f), t)
    parseTuple t = unexpected t "tuple"

    unexpected :: Show a => a -> String -> ExceptT String m b
    unexpected actual expected = throwError $ concat [
          "Unexpected "
        , show actual
        , " (expected "
        , expected
        , ")"
        ]
