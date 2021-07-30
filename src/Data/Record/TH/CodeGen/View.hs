{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | View on a TH record declaration
--
-- Rather than working with the TH types directly, we work with our own view,
-- which results in slightly clearer code.
module Data.Record.TH.CodeGen.View (
    -- * View
    Record(..)
  , Field(..)
  , Deriving(..)
    -- * Constructing the view
  , matchRecord
  ) where

import Data.Maybe (catMaybes)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Record.TH.CodeGen.Name (TypeName, ConstrName, FieldName)
import Data.Record.TH.CodeGen.TH
import Data.Record.TH.CodeGen.Util

import qualified Data.Record.TH.CodeGen.Name as N
import Data.Either (partitionEithers)

{-------------------------------------------------------------------------------
  View
-------------------------------------------------------------------------------}

-- | Our internal representation of a record
--
-- We parse the 'Dec' that TH gives us and represent it in a more convenient way
-- for processing. For our running example, the internal representation looks
-- like this:
--
-- > Record {
-- >     recordUnqual = "T"
-- >   , recordConstr = "MkT"
-- >   , recordFields = [
-- >         Field {fieldUnqual = "tInt"   , fieldType = ConT 'Word          , fieldIndex = 0}
-- >       , Field {fieldUnqual = "tBool"  , fieldType = ConT 'Bool          , fieldIndex = 1}
-- >       , Field {fieldUnqual = "tChar"  , fieldType = ConT 'Char          , fieldIndex = 2}
-- >       , Field {fieldUnqual = "tA"     , fieldType = VarT a              , fieldIndex = 3}
-- >       , Field {fieldUnqual = "tListB" , fieldType = AppT ListT (VarT b) , fieldIndex = 4}
-- >       ]
-- >   , recordDeriv = [DeriveEq, DeriveShow]
-- >   , recordTVars = [PlainTV a, PlainTV b]
-- >   }
data Record = Record {
      -- | Name of the record /type/
      recordUnqual :: TypeName 'N.Unique

      -- | The type variables in the records type
    , recordTVars :: [TyVarBndr]

      -- | Unqualified name of the record /constructor/
    , recordConstr :: ConstrName 'N.Unique

      -- | The fields in the record
    , recordFields :: [Field]

      -- | Explicitly supported type class instances
    , recordDeriv :: [Deriving]

      -- | Anyclass deriving
      --
      -- We list these separately, because we need to add these as anyclass
      -- deriving classes when defining the newtype, rather than as standalone
      -- deriving instances. (If we don't, we need to duplicate ghc's logic for
      -- figuring out how many parameters to provide to the datatype.)
    , recordAnyclass :: [Type]
    }
  deriving (Show)

data Field = Field {
      -- | Unqualified name of the field
      fieldUnqual :: FieldName

      -- | Type of the field
    , fieldType :: Type

      -- | Index of the field (field 0, field 1, ..)
      --
      -- This is strictly speaking redundant information, as this is already
      -- implied by the position of the field in 'recordFields'. However, since
      -- we do a lot of positional processing (every field corresponds to a
      -- vector element), it is convenient to have the index readily available.
    , fieldIndex  :: Int
    }
  deriving (Show)

data Deriving =
    DeriveEq
  | DeriveOrd
  | DeriveShow
  deriving (Show)

{-------------------------------------------------------------------------------
  Constructing the view
-------------------------------------------------------------------------------}

-- | Try to match a record declaration
--
-- We use 'Maybe' in these matching functions, along with 'reportError', so that
-- we can report multiple errors rather than stopping at the first.
matchRecord :: Dec -> Q (Maybe Record)
matchRecord (DataD
               _cxt@[]
               typeName
               tyVarBndrs
               _kind@Nothing
               [RecC constrName fieldTypes]
               derivClauses
            ) = do
    fields <- catMaybes <$> mapM matchField (zip [0..] fieldTypes)
    (deriv, anyclass) <- partitionEithers <$> concatMapM matchDeriv derivClauses
    return $ Just Record {
        recordUnqual   = N.TypeName $ N.fromName' typeName
      , recordTVars    = tyVarBndrs
      , recordConstr   = N.ConstrName $ N.fromName' constrName
      , recordFields   = fields
      , recordDeriv    = deriv
      , recordAnyclass = anyclass
      }
matchRecord d = do
    reportError $ "Unsupported declaration: " ++ show d
    return Nothing

-- | Support deriving clauses
--
-- We return the anyclass deriving clauses separately.
-- See 'recordAnyclass' for more details.
matchDeriv :: DerivClause -> Q [Either Deriving Type]
matchDeriv = \case
    DerivClause Nothing cs ->
      map Left <$> derivStock cs
    DerivClause (Just StockStrategy) cs ->
      map Left <$> derivStock cs
    DerivClause (Just AnyclassStrategy) cs ->
      return $ map Right cs
    DerivClause strategy _ -> do
      reportError $ "Unsupported deriving strategy " ++ show strategy
      return []
  where
    derivStock cs = catMaybes <$> mapM go cs
    go :: Pred -> Q (Maybe Deriving)
    go p | p == ConT ''Eq   = return $ Just DeriveEq
         | p == ConT ''Ord  = return $ Just DeriveOrd
         | p == ConT ''Show = return $ Just DeriveShow
         | otherwise        = do
             reportError $ "Cannot derive instance for " ++ show p
             return Nothing

matchField :: (Int, VarBangType) -> Q (Maybe Field)
matchField (i, (fieldName, bng, typ)) =
    case bng of
      DefaultBang ->
        return . Just $ Field (unqualify fieldName) typ i
      _otherwise  -> do
        reportError $ "Unsupported bang type: " ++ show bng
        return Nothing
  where
    unqualify :: Name -> FieldName
    unqualify = N.FieldName . N.OverloadedName . undoDRF . nameBase

-- When @DuplicateRecordFields@ is enabled, it produces field names such as
-- @$sel:a:MkY@. We don't really care much about 'DuplicateRecordFields',
-- insofar as that we will not try to be compatible with DRF-style
-- overloading (all overloading must happen through 'HasField' instead).
-- We do however need to recover the original field name.
--
-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/records/overloaded-record-fields/duplicate-record-fields>
-- <https://gitlab.haskell.org/ghc/ghc/-/issues/14848>
undoDRF :: String -> String
undoDRF fieldName =
   case fieldName of
     '$' : drf  -> takeWhile (/= ':') . tail . dropWhile (/= ':') $ drf
     _otherwise -> fieldName
