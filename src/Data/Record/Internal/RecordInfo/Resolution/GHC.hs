{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Data.Record.Internal.RecordInfo.Resolution.GHC (
    parseRecordInfo
  ) where

import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Data.Void
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Control.Monad.Except as Except

import Data.Record.Generic
import Data.Record.Internal.RecordInfo
import Data.Record.Internal.TH.Util

import qualified Data.Record.Internal.TH.Name as N

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | Parse previously constructed type level data
--
-- We do this when we construct record /values/, at which point we have no
-- 'Options', so this must work without options.
--
-- 'Nothing' if this wasn't a type created using @large-records@.
parseRecordInfo :: forall m.
     Quasi m
  => N.Name 'N.DataName 'N.Global
  -> m (Either String (RecordInfo Void))
parseRecordInfo constr = runExceptT $ do
    parent    <- Except.lift (N.reify constr) >>= getDataConParent
    saturated <- Except.lift (N.reify parent) >>= getSaturatedType
    parsed    <- Except.lift (getMetadataInstance saturated) >>= parseTySynInst
    return $ mkRecordInfo parent parsed
  where
    mkRecordInfo ::
         N.Name 'N.TcClsName 'N.Global
      -> ([TyVarBndr], [(N.OverloadedName, Type)])
      -> RecordInfo Void
    mkRecordInfo rType (tyVars, fieldTypes) = RecordInfo {
          recordInfoUnqual = rType
        , recordInfoTVars  = tyVars
        , recordInfoConstr = constr
        , recordInfoFields = zipWith (uncurry mkFieldInfo) fieldTypes [0..]
        }

    mkFieldInfo :: N.OverloadedName -> Type -> Int -> FieldInfo Void
    mkFieldInfo fName fType ix = FieldInfo {
          fieldInfoUnqual = fName
        , fieldInfoType   = fType
        , fieldInfoIndex  = ix
        , fieldInfoVal    = Nothing
        }

    saturate :: Name -> [TyVarBndr] -> Type
    saturate n = foldl (\t v -> t `AppT` VarT (tyVarName v)) (ConT n)

    getMetadataInstance :: Type -> m [InstanceDec]
    getMetadataInstance = runQ . reifyInstances ''MetadataOf . (:[])

    getSaturatedType :: Info -> ExceptT String m Type
    getSaturatedType (TyConI (NewtypeD [] nm tyVars _kind _con _deriv)) =
        return $ saturate nm tyVars
    getSaturatedType i =
        unexpected i "newtype"

    getDataConParent :: Info -> ExceptT String m (N.Name 'N.TcClsName 'N.Global)
    getDataConParent (DataConI _ _ parent) =
        return $ N.fromName' parent
    getDataConParent i =
        unexpected i "data constructor"

    parseTySynInst ::
         [InstanceDec]
      -> ExceptT String m ([TyVarBndr], [(N.OverloadedName, Type)])
    parseTySynInst [TySynInstD (TySynEqn vars _lhs rhs)] =
        (fromMaybe [] vars, ) <$> parseList rhs
    parseTySynInst is =
        unexpected is "type instance"

    parseList :: Type -> ExceptT String m [(N.OverloadedName, Type)]
    parseList (AppT (AppT PromotedConsT t) ts) =
        (:) <$> parseTuple t <*> parseList ts
    parseList PromotedNilT =
        return []
    parseList (SigT t _kind) =
        parseList t
    parseList t = unexpected t "list"

    parseTuple :: Type -> ExceptT String m (N.OverloadedName, Type)
    parseTuple (AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit f))) t) =
        return (N.OverloadedName f, t)
    parseTuple t = unexpected t "tuple"

    unexpected :: Show a => a -> String -> ExceptT String m b
    unexpected actual expected = throwError $ concat [
          "Unexpected "
        , show actual
        , " (expected "
        , expected
        , ")"
        ]
