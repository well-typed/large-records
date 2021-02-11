{-# LANGUAGE ParallelListComp #-}

-- | Our view on record expressions/patterns
--
-- This follows "Data.Record.TH.CodeGen.View" closely, but lacks global info
-- about the type (e.g., which class instances it derives). The only type info
-- we /do/ have is whatever we generate as part of the type-level metadata.
module Data.Record.QQ.CodeGen.View (
    -- * View
    Record(..)
  , Field(..)
    -- * Constructing the view
  , matchRecordExp
  , matchRecordPat
  ) where

import Data.Bifunctor (first)
import Control.Monad.Except
import Language.Haskell.TH

import qualified Data.Map as Map

import Data.Record.TH.CodeGen.Name
import Data.Record.QQ.CodeGen.Metadata

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Record a = Record {
      recordUnqual :: TypeName
    , recordTVars  :: [TyVarBndr]
    , recordConstr :: ConstrName
    , recordFields :: [Field a]
    }
  deriving (Show)

data Field a = Field {
      fieldUnqual :: FieldName
    , fieldType   :: Type
    , fieldIndex  :: Int
    , fieldDec    :: Maybe a -- ^ Nothing if not present
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Constructing the view
-------------------------------------------------------------------------------}

matchRecordExp :: Exp -> Q (Maybe (Record Exp))
matchRecordExp = matchRecord termExp

matchRecordPat :: Pat -> Q (Maybe (Record Pat))
matchRecordPat = matchRecord termPat

matchRecord ::
     (a -> Maybe (ConstrName, [(FieldName, a)]))
  -> a -> Q (Maybe (Record a))
matchRecord term a =
    case term a of
      Nothing ->
        -- Not a record term/pattern
        return Nothing
      Just (constr, fields) -> do
        let fields' = Map.fromList fields
        mMetadata <- runExceptT $ getTypeLevelMetadata constr
        case mMetadata of
          Left _err ->
            -- If we can't find any metadata, assume it's a regular record
            -- rather than a @large-records@ record, and return 'Nothing'
            return Nothing
          Right (typeName, (tyVars, fieldTypes)) ->
            return $ Just Record {
                recordUnqual = typeName
              , recordTVars  = tyVars
              , recordConstr = constr
              , recordFields = [
                    Field {
                        fieldUnqual = fieldName
                      , fieldType   = typ
                      , fieldIndex  = ix
                      , fieldDec    = Map.lookup fieldName fields'
                      }
                  | (fieldName, typ) <- fieldTypes
                  | ix <- [0..]
                  ]
              }

termExp :: Exp -> Maybe (ConstrName, [(FieldName, Exp)])
termExp (RecConE constr fields) = Just (
      ConstrName (nameBase constr)
    , map (first (FieldName . nameBase)) fields
    )
termExp _otherwise = Nothing

termPat :: Pat -> Maybe (ConstrName, [(FieldName, Pat)])
termPat (RecP constr fields) = Just (
      ConstrName (nameBase constr)
    , map (first (FieldName . nameBase)) fields
    )
termPat _otherwise = Nothing
