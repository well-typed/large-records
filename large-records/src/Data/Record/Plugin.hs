{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}

-- | Support for scalable large records
--
-- = Usage
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}
-- >
-- > import Data.Record.Plugin
-- >
-- > {-# ANN type B largeRecord #-}
-- > data B a = B {a :: a, b :: String}
-- >   deriving stock (Show, Eq, Ord)
--
-- See 'LargeRecordOptions' for the list of all possible annotations.
--
-- = Usage with @record-dot-preprocessor@
--
-- The easiest way to use both plugins together is to do
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Plugin.WithRDP #-}
--
-- You /can/ also load them separately, but if you do, you need to be careful
-- with the order. Unfortunately, the correct order is different in different
-- ghc versions. Prior to ghc 9.4, the plugins must be loaded like this:
--
-- > {-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin #-}
--
-- From ghc 9.4 and up, they need to be loaded in the opposite order:
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Plugin -fplugin=RecordDotPreprocessor #-}
module Data.Record.Plugin (
    -- * Annotations
    LargeRecordOptions(..)
  , largeRecord
    -- * For use by ghc
  , plugin
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Writer.CPS (WriterT, tell, runWriterT)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Traversable (for)
import Language.Haskell.TH (Extension(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Data.Record.Internal.Plugin.CodeGen (genLargeRecord)
import Data.Record.Internal.GHC.Fresh
import Data.Record.Internal.GHC.Shim
import Data.Record.Internal.GHC.TemplateHaskellStyle
import Data.Record.Internal.Plugin.Exception
import Data.Record.Internal.Plugin.Options
import Data.Record.Internal.Plugin.Record
import Data.Record.Internal.Plugin.Names

#if __GLASGOW_HASKELL__ >= 902
import GHC.Utils.Logger (getLogger)
#endif

#if __GLASGOW_HASKELL__ == 902
import GHC.Types.Error (mkWarnMsg, mkErr, mkDecorated)
import GHC.Driver.Errors (printOrThrowWarnings)
#endif

#if __GLASGOW_HASKELL__ >= 904
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (GhcMessage(GhcUnknownMessage))
import GHC.Types.Error (mkPlainError, mkMessages, mkPlainDiagnostic)
import GHC.Utils.Error (mkMsgEnvelope, mkErrorMsgEnvelope)
#endif

#if __GLASGOW_HASKELL__ >= 906
import GHC.Types.Error (UnknownDiagnostic(..))
import GHC.Driver.Config.Diagnostic (initPrintConfig)
#endif

{-------------------------------------------------------------------------------
  Top-level: the plugin proper
-------------------------------------------------------------------------------}

plugin :: Plugin
plugin = defaultPlugin {
      parsedResultAction = \_ _ -> ignoreMessages aux
    , pluginRecompile    = purePlugin
    }
  where
#if __GLASGOW_HASKELL__ >= 904
    ignoreMessages f (ParsedResult modl msgs) =
            (\modl' -> ParsedResult modl' msgs) <$> f modl
#else
    ignoreMessages = id
#endif

    aux :: HsParsedModule -> Hsc HsParsedModule
    aux parsed@HsParsedModule{hpm_module = modl} = do
        modl' <- transformDecls modl
        pure $ parsed { hpm_module = modl' }

{-------------------------------------------------------------------------------
  Transform datatype declarations
-------------------------------------------------------------------------------}

transformDecls :: LHsModule -> Hsc LHsModule
transformDecls (L l modl@HsModule{hsmodDecls = decls}) = do
    (decls', transformed) <- runWriterT $ for decls $ transformDecl largeRecords

    checkEnabledExtensions l

    -- Check for annotations without corresponding types
    let untransformed = Map.keysSet largeRecords `Set.difference` transformed
    unless (Set.null untransformed) $ do
      issueError l $ vcat $
          text "These large-record annotations were not applied:"
        : [text (" - " ++ n) | n <- Set.toList untransformed]

    -- We add imports whether or not there were some errors, to avoid spurious
    -- additional errors from ghc about things not in scope.
    pure $ L l $ modl{hsmodDecls = concat decls'}
  where
    largeRecords :: Map String [(SrcSpan, LargeRecordOptions)]
    largeRecords = getLargeRecordOptions modl

transformDecl ::
     Map String [(SrcSpan, LargeRecordOptions)]
  -> LHsDecl GhcPs
  -> WriterT (Set String) Hsc [LHsDecl GhcPs]
transformDecl largeRecords decl@(reLoc -> L l _) =
    case decl of
      DataD (nameBase -> name) _ _ _  ->
        case Map.findWithDefault [] name largeRecords of
          [] ->
            -- Not a large record. Leave alone.
            return [decl]
          (_:_:_) -> do
            lift $ issueError l $ text ("Conflicting annotations for " ++ name)
            return [decl]
          [(annLoc, opts)] -> do
            tell (Set.singleton name)
            case runExcept (viewRecord annLoc opts decl) of
              Left e -> do
                lift $ issueError (exceptionLoc e) (exceptionToSDoc e)
                -- Return the declaration unchanged if we cannot parse it
                return [decl]
              Right r -> lift $ do
                dynFlags <- getDynFlags
                names    <- getQualifiedNames
                newDecls <- runFreshHsc $ genLargeRecord names r dynFlags
                when (debugLargeRecords opts) $
                  issueWarning l (debugMsg newDecls)
                pure newDecls
      _otherwise ->
        pure [decl]
  where
    debugMsg :: [LHsDecl GhcPs] -> SDoc
    debugMsg newDecls = pprSetDepth AllTheWay $ vcat $
          text "large-records: splicing in the following definitions:"
        : map ppr newDecls

{-------------------------------------------------------------------------------
  Check for enabled extensions

  In ghc 8.10 and up there are DynFlags plugins, which we could use to enable
  these extensions for the user. Since this is not available in 8.8 however we
  will not make use of this for now. (There is also reason to believe that these
  may be removed again in later ghc releases.)
-------------------------------------------------------------------------------}

checkEnabledExtensions :: SrcSpan -> Hsc ()
checkEnabledExtensions l = do
    dynFlags <- getDynFlags
    let missing :: [RequiredExtension]
        missing = filter (not . isEnabled dynFlags) requiredExtensions
    unless (null missing) $
      -- We issue a warning here instead of an error, for better integration
      -- with HLS. Frankly, I'm not entirely sure what's going on there.
      issueWarning l $ vcat . concat $ [
          [text "Please enable these extensions for use with large-records:"]
        , map ppr missing
        ]
  where
    requiredExtensions :: [RequiredExtension]
    requiredExtensions = [
          RequiredExtension [ConstraintKinds]
        , RequiredExtension [DataKinds]
        , RequiredExtension [ExistentialQuantification, GADTs]
        , RequiredExtension [FlexibleInstances]
        , RequiredExtension [MultiParamTypeClasses]
        , RequiredExtension [ScopedTypeVariables]
        , RequiredExtension [TypeFamilies]
        , RequiredExtension [UndecidableInstances]
        ]

-- | Required extension
--
-- The list is used to represent alternative extensions that could all work
-- (e.g., @GADTs@ and @ExistentialQuantification@).
data RequiredExtension = RequiredExtension [Extension]

instance Outputable RequiredExtension where
  ppr (RequiredExtension exts) = hsep . intersperse (text "or") $ map ppr exts

isEnabled :: DynFlags -> RequiredExtension -> Bool
isEnabled dynflags (RequiredExtension exts) = any (`xopt` dynflags) exts

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

issueError :: SrcSpan -> SDoc -> Hsc ()
issueError l errMsg = do
#if __GLASGOW_HASKELL__ == 902
    throwOneError $
      mkErr l neverQualify (mkDecorated [errMsg])
#elif __GLASGOW_HASKELL__ >= 906
    throwOneError $
      mkErrorMsgEnvelope
        l
        neverQualify
        (GhcUnknownMessage $ UnknownDiagnostic $ mkPlainError [] errMsg)
#elif __GLASGOW_HASKELL__ >= 904
    throwOneError $
      mkErrorMsgEnvelope
        l
        neverQualify
        (GhcUnknownMessage $ mkPlainError [] errMsg)
#else
    dynFlags <- getDynFlags
    throwOneError $
      mkErrMsg dynFlags l neverQualify errMsg
#endif

issueWarning :: SrcSpan -> SDoc -> Hsc ()
issueWarning l errMsg = do
    dynFlags <- getDynFlags
#if __GLASGOW_HASKELL__ == 902
    logger <- getLogger
    liftIO $ printOrThrowWarnings logger dynFlags . bag $
      mkWarnMsg l neverQualify errMsg
#elif __GLASGOW_HASKELL__ >= 906
    logger <- getLogger
    dflags <- getDynFlags
    let print_config = initPrintConfig dflags
    liftIO $ printOrThrowDiagnostics logger print_config (initDiagOpts dynFlags) . mkMessages . bag $
      mkMsgEnvelope
        (initDiagOpts dynFlags)
        l
        neverQualify
        (GhcUnknownMessage $ UnknownDiagnostic $ mkPlainDiagnostic WarningWithoutFlag [] errMsg)
#elif __GLASGOW_HASKELL__ >= 904
    logger <- getLogger
    liftIO $ printOrThrowDiagnostics logger (initDiagOpts dynFlags) . mkMessages . bag $
      mkMsgEnvelope
        (initDiagOpts dynFlags)
        l
        neverQualify
        (GhcUnknownMessage $ mkPlainDiagnostic WarningWithoutFlag [] errMsg)
#else
    liftIO $ printOrThrowWarnings dynFlags . bag $
      mkWarnMsg dynFlags l neverQualify errMsg
#endif
  where
    bag :: a -> Bag a
    bag = listToBag . (:[])
