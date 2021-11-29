{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Record.Internal.Record.Parser (
    RecordInstances(..)
  , Deriving(..)
  , parseRecordDef
  ) where

import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Record.Internal.Record
import Data.Record.Internal.TH.Util
import Data.Record.Internal.Util

{-------------------------------------------------------------------------------
  Record instances
-------------------------------------------------------------------------------}

data RecordInstances = RecordInstances {
      -- | Explicitly supported type class instances
      recordInstancesDerived :: [Deriving]

      -- | Non-stock deriving
      --
      -- We list these separately, because we need to add these as anyclass
      -- deriving classes when defining the newtype, rather than as standalone
      -- deriving instances. (If we don't, we need to duplicate ghc's logic for
      -- figuring out how many parameters to provide to the datatype.)
    , recordInstancesNonstock :: [(DerivStrategy, Type)]
    }

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
parseRecordDef :: Dec -> Q (Maybe (Record (), RecordInstances))
parseRecordDef (DataD
                  _cxt@[]
                  typeName
                  tyVarBndrs
                  _kind@Nothing
                  [RecC constrName fieldTypes]
                  derivClauses
               ) = do

    fields            <- catMaybes <$>
                           mapM parseFieldDef (zip [0..] fieldTypes)
    (stock, nonstock) <- partitionEithers <$>
                           concatMapM parseDeriv derivClauses

    return $ Just (
        Record {
            recordType   = nameBase typeName
          , recordConstr = nameBase constrName
          , recordTVars  = tyVarBndrs
          , recordFields = fields
          }
      , RecordInstances {
            recordInstancesDerived  = stock
          , recordInstancesNonstock = nonstock
          }
      )
parseRecordDef d = do
    reportError $ "Unsupported declaration: " ++ show d
    return Nothing

-- | Support deriving clauses
--
-- We return the anyclass deriving clauses separately.
-- See 'recordAnyclass' for more details.
parseDeriv :: DerivClause -> Q [Either Deriving (DerivStrategy, Type)]
parseDeriv = \case
    DerivClause Nothing cs ->
      map Left <$> derivStock cs
    DerivClause (Just StockStrategy) cs ->
      map Left <$> derivStock cs
    DerivClause (Just strategy) cs ->
      return $ map (Right . (,)  strategy) cs
  where
    derivStock cs = catMaybes <$> mapM go cs
    go :: Pred -> Q (Maybe Deriving)
    go p | p == ConT ''Eq   = return $ Just DeriveEq
         | p == ConT ''Ord  = return $ Just DeriveOrd
         | p == ConT ''Show = return $ Just DeriveShow
         | otherwise        = do
             reportError $ "Cannot derive instance for " ++ show p
             return Nothing

parseFieldDef :: (Int, VarBangType) -> Q (Maybe (Field ()))
parseFieldDef (i, (nm, bng, typ)) =
    case bng of
      DefaultBang ->
        return . Just $ Field {
            fieldName  = unqualify nm
          , fieldType  = typ
          , fieldIndex = i
          , fieldVal   = ()
          }
      _otherwise  -> do
        reportError $ "Unsupported bang type: " ++ show bng
        return Nothing
  where
    unqualify :: Name -> String
    unqualify = undoDRF . nameBase

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- When @DuplicateRecordFields@ is enabled, it produces field names such as
-- @$sel:a:MkY@. We don't really care much about 'DuplicateRecordFields',
-- insofar as that we will not try to be compatible with DRF-style
-- overloading (all overloading must happen through 'HasField' instead).
-- We do however need to recover the original field name.
--
-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/records/overloaded-record-fields/duplicate-record-fields>
-- <https://gitlab.haskell.org/ghc/ghc/-/issues/14848>
undoDRF :: String -> String
undoDRF nm =
   case nm of
     '$' : drf  -> takeWhile (/= ':') . tail . dropWhile (/= ':') $ drf
     _otherwise -> nm
