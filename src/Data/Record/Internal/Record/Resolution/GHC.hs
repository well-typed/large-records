{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Data.Record.Internal.Record.Resolution.GHC (
    parseRecordInfo
  ) where

import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Language.Haskell.TH hiding (TyVarBndr)
import Language.Haskell.TH.Syntax hiding (TyVarBndr)

import qualified Control.Monad.Except as Except

import Data.Record.Generic
import Data.Record.Internal.Record
import Data.Record.Internal.TH.Compat
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
  => String                       -- ^ User-defined constructor
  -> N.Name 'DataName 'N.Global   -- ^ Internal constructor
  -> ExceptT String m (Record ())
parseRecordInfo userConstr internalConstr = do
    parent    <- Except.lift (N.reify internalConstr) >>= getDataConParent
    saturated <- Except.lift (N.reify parent) >>= getSaturatedType
    parsed    <- Except.lift (getMetadataInstance saturated) >>= parseTySynInst
    return $ mkRecordInfo (N.nameBase parent) parsed
  where
    mkRecordInfo ::
         String
      -> ([TyVarBndr], [(String, Type)])
      -> Record ()
    mkRecordInfo rType (tyVars, fieldTypes) = Record {
          recordType   = rType
        , recordConstr = userConstr
        , recordTVars  = tyVars
        , recordFields = zipWith (uncurry mkFieldInfo) fieldTypes [0..]
        }

    mkFieldInfo :: String -> Type -> Int -> Field ()
    mkFieldInfo fName fType ix = Field {
          fieldName  = fName
        , fieldType  = fType
        , fieldIndex = ix
        , fieldVal   = ()
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

    getDataConParent :: Info -> ExceptT String m (N.Name 'TcClsName 'N.Global)
    getDataConParent (DataConI _ _ parent) =
        return $ N.fromTH' parent
    getDataConParent i =
        unexpected i "data constructor"

    parseTySynInst ::
         [InstanceDec]
      -> ExceptT String m ([TyVarBndr], [(String, Type)])
    parseTySynInst [TySynInstD (TySynEqn vars _lhs rhs)] =
        (fromMaybe [] vars, ) <$> parseList rhs
    parseTySynInst is =
        unexpected is "type instance"

    parseList :: Type -> ExceptT String m [(String, Type)]
    parseList (AppT (AppT PromotedConsT t) ts) =
        (:) <$> parseTuple t <*> parseList ts
    parseList PromotedNilT =
        return []
    parseList (SigT t _kind) =
        parseList t
    parseList t = unexpected t "list"

    parseTuple :: Type -> ExceptT String m (String, Type)
    parseTuple (AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit f))) t) =
        return (f, t)
    parseTuple t = unexpected t "tuple"

    unexpected :: Show a => a -> String -> ExceptT String m b
    unexpected actual expected = throwError $ concat [
          "Unexpected "
        , show actual
        , " (expected "
        , expected
        , ")"
        ]
