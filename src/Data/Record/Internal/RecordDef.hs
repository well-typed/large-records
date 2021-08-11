{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Record.Internal.RecordDef (
    -- * Definition
    RecordDef(..)
  , FieldDef(..)
  , Deriving(..)
    -- * Parsing
  , parseRecordDef
  ) where

import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Record.Internal.Util
import Data.Record.Internal.TH.Util

import qualified Data.Record.Internal.TH.Name as N

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Our internal representation of a record definition
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
data RecordDef = RecordDef {
      -- | Name of the record /type/
      recordDefUnqual :: N.Name 'TcClsName 'N.Unique

      -- | The type variables in the records type
    , recordDefTVars :: [TyVarBndr]

      -- | Unqualified name of the record /constructor/
    , recordDefConstr :: N.Name 'N.DataName 'N.Unique

      -- | The fields in the record
    , recordDefFields :: [FieldDef]

      -- | Explicitly supported type class instances
    , recordDefDeriv :: [Deriving]

      -- | Anyclass deriving
      --
      -- We list these separately, because we need to add these as anyclass
      -- deriving classes when defining the newtype, rather than as standalone
      -- deriving instances. (If we don't, we need to duplicate ghc's logic for
      -- figuring out how many parameters to provide to the datatype.)
    , recordDefAnyclass :: [Type]
    }
  deriving (Show)

data FieldDef = FieldDef {
      -- | Unqualified name of the field
      fieldDefUnqual :: N.OverloadedName

      -- | Type of the field
    , fieldDefType :: Type

      -- | Index of the field (field 0, field 1, ..)
      --
      -- This is strictly speaking redundant information, as this is already
      -- implied by the position of the field in 'recordFields'. However, since
      -- we do a lot of positional processing (every field corresponds to a
      -- vector element), it is convenient to have the index readily available.
    , fieldDefIndex  :: Int
    }
  deriving (Show)

data Deriving =
    DeriveEq
  | DeriveOrd
  | DeriveShow
  deriving (Show)

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | Try to match a record declaration
--
-- We use 'Maybe' in these matching functions, along with 'reportError', so that
-- we can report multiple errors rather than stopping at the first.
parseRecordDef :: Dec -> Q (Maybe RecordDef)
parseRecordDef (DataD
                  _cxt@[]
                  typeName
                  tyVarBndrs
                  _kind@Nothing
                  [RecC constrName fieldTypes]
                  derivClauses
               ) = do
    fields <- catMaybes <$> mapM parseFieldDef (zip [0..] fieldTypes)
    (deriv, anyclass) <- partitionEithers <$> concatMapM parseDeriv derivClauses
    return $ Just RecordDef {
        recordDefUnqual   = N.fromName' typeName
      , recordDefTVars    = tyVarBndrs
      , recordDefConstr   = N.fromName' constrName
      , recordDefFields   = fields
      , recordDefDeriv    = deriv
      , recordDefAnyclass = anyclass
      }
parseRecordDef d = do
    reportError $ "Unsupported declaration: " ++ show d
    return Nothing

-- | Support deriving clauses
--
-- We return the anyclass deriving clauses separately.
-- See 'recordAnyclass' for more details.
parseDeriv :: DerivClause -> Q [Either Deriving Type]
parseDeriv = \case
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

parseFieldDef :: (Int, VarBangType) -> Q (Maybe FieldDef)
parseFieldDef (i, (fieldName, bng, typ)) =
    case bng of
      DefaultBang ->
        return . Just $ FieldDef (unqualify fieldName) typ i
      _otherwise  -> do
        reportError $ "Unsupported bang type: " ++ show bng
        return Nothing
  where
    unqualify :: Name -> N.OverloadedName
    unqualify = N.OverloadedName . undoDRF . nameBase

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
