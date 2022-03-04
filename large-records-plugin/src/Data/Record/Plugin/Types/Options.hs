{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generation options for large-records.
module Data.Record.Plugin.Types.Options
  ( LargeRecordOptions (..),
    shouldRecordBeStrict,
    shouldGeneratedHasField,
    getLargeRecordOptions,
  )
where

import Control.Applicative (empty)
import Data.Data (Data)
import Data.Map (Map)

import GHC (HsModule)

import qualified Data.Generics.Uniplate.Data as Uniplate
import qualified Data.Map.Strict             as Map

import Data.Record.Plugin.GHC.TemplateHaskellStyle

-- | A type specifying how a record should be treated by large-records.
--
-- The only possible annotations are currently:
--
-- > {-# ANN type T LargeRecordStrict #-}
-- > {-# ANN type T LargeRecordLazy #-}
data LargeRecordOptions = LargeRecordStrict | LargeRecordLazy
  deriving stock (Data)

-- | Whether all fields of the record should be strict.
shouldRecordBeStrict :: LargeRecordOptions -> Bool
shouldRecordBeStrict LargeRecordStrict = True
shouldRecordBeStrict LargeRecordLazy = False

-- | Whether we should generate @HasField@ instances. Currently always 'True'.
shouldGeneratedHasField :: LargeRecordOptions -> Bool
shouldGeneratedHasField _ = True

-- | Extract 'LargeRecordOptions' for all types with large-records pragmas in the module.
getLargeRecordOptions :: HsModule GhcPs -> Map RdrName LargeRecordOptions
getLargeRecordOptions module_ = Map.fromList do
  anno <- Uniplate.universeBi module_
  case viewLargeRecordAnnotation anno of
    Just (tyName, option) -> pure (tyName, option)
    Nothing -> empty

viewLargeRecordAnnotation :: AnnDecl GhcPs -> Maybe (RdrName, LargeRecordOptions)
viewLargeRecordAnnotation = \case
    PragAnnD (TypeAnnotation tyName) (VarE (nameBase -> "LargeRecordStrict")) ->
      Just (tyName, LargeRecordStrict)
    PragAnnD (TypeAnnotation tyName) (VarE (nameBase -> "LargeRecordLazy")) ->
      Just (tyName, LargeRecordLazy)
    _ -> Nothing

