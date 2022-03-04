{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- | A GHC plugin that gives the large-records treatment to records with special annotations.
--
-- = Usage
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}
-- >
-- > {-# ANN type B LargeRecordStrict #-}
-- > data B a = B {a :: a, b :: String}
-- >   deriving stock (Show, Eq, Ord)
--
-- See 'LargeRecordOptions' for the list of all possible annotations.
--
-- = Usage with record-dot-preprocessor
--
-- There are two important points. First, the order of plugins matters — record-dot-preprocessor has to be listed before this plugin (and
-- correspondingly will be applied /after/ this plugin):
--
-- > {-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin #-}
--
-- Second, for now the official version of record-dot-preprocessor does not work with this plugin. Use the patched version from this pull request:
-- <https://github.com/ndmitchell/record-dot-preprocessor/pull/48>.

module Data.Record.Plugin (plugin, LargeRecordOptions (..)) where

import Prelude hiding (mod)

import Control.Monad.Except
import Control.Monad.Writer
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import Data.Record.Plugin.CodeGen (genLargeRecord)
import Data.Record.Plugin.GHC
import Data.Record.Plugin.RuntimeNames (allRuntimeModules)
import Data.Record.Plugin.Types.Exception
import Data.Record.Plugin.Types.Options (LargeRecordOptions (..), getLargeRecordOptions)
import Data.Record.Plugin.Types.Record (viewDataDeclName, viewRecord)
import qualified Data.Set as Set
import Data.Traversable (for)

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction, pluginRecompile = purePlugin}
  where
    parsedResultAction _options _ mod@HsParsedModule {hpm_module = L l module_} = do
      case runExcept (transformDecls module_) of
        Right module_' -> do
          pure mod {hpm_module = L l (addRequiredImports module_')}
        Left err -> do
          dynFlags <- getDynFlags
          error (formatException dynFlags err)

transformDecls :: HsModule GhcPs -> Except Exception (HsModule GhcPs)
transformDecls mod@HsModule {hsmodDecls = decls} = do
  let largeRecords = getLargeRecordOptions mod

  (fold -> decls', transformed) <- runWriterT $ for decls \decl ->
    case viewDataDeclName decl of
      Just tyName | Just opts <- tyName `Map.lookup` largeRecords -> do
        tell (Set.singleton tyName)
        rec <- lift (viewRecord opts decl)
        pure (genLargeRecord rec)
      _ -> pure [decl]

  let untransformed = Map.keysSet largeRecords `Set.difference` transformed
  unless (Set.null untransformed) do
    throwError (Untransformed untransformed)

  pure mod {hsmodDecls = decls'}

addRequiredImports :: HsModule GhcPs -> HsModule GhcPs
addRequiredImports module_@HsModule {hsmodImports} =
  module_ {hsmodImports = hsmodImports ++ [qimportD m | m <- allRuntimeModules]}

