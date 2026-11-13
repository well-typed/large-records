{-# LANGUAGE CPP #-}

module Data.Record.Anon.Internal.Plugin.Source (sourcePlugin) where

import Control.Monad
import Control.Monad.Trans
import Data.Generics (everywhereM, mkM)

import Data.Record.Anon.Internal.Plugin.Source.FreshT
import Data.Record.Anon.Internal.Plugin.Source.GhcShim
import Data.Record.Anon.Internal.Plugin.Source.Names
import Data.Record.Anon.Internal.Plugin.Source.Options

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

sourcePlugin :: [String] -> HsParsedModule -> Hsc HsParsedModule
sourcePlugin rawOpts
             parsed@HsParsedModule{
                 hpm_module = L l modl@HsModule{hsmodDecls = decls}
               } = do

    decls' <- runFreshHsc $
                everywhereM
                  (mkM $ transformExpr opts)
                  decls
    return $ parsed {
        hpm_module = L l $ modl { hsmodDecls = decls' }
      }
  where
    opts :: Options
    opts = parseOpts rawOpts

transformExpr :: Options -> LHsExpr GhcPs -> FreshT Hsc (LHsExpr GhcPs)
transformExpr options@Options{debug} e@(reLoc -> L l expr)
  | RecordCon _ext (L _ nm) HsRecFields{rec_flds, rec_dotdot} <- expr
  , Unqual nm' <- nm
  , Nothing    <- rec_dotdot
  , Just mode  <- parseMode (occNameString nm')
  , Just flds' <- mapM getField rec_flds
  = do names <- lift $ getLargeAnonNames mode
       e'    <- anonRec options names l flds'
       when debug $ lift $ issueWarning l (debugMsg e')
       return e'

  | otherwise
  = return e
  where
    getField ::
         LHsRecField GhcPs (LHsExpr GhcPs)
      -> Maybe (FastString, LHsExpr GhcPs)
#if __GLASGOW_HASKELL__ < 904
    getField (L _ (HsRecField
                    { hsRecFieldLbl = L _ fieldOcc
                    , hsRecFieldArg = arg
                    , hsRecPun = pun }))
#else
    getField (L _ (HsFieldBind
                    { hfbLHS = L _ fieldOcc
                    , hfbRHS = arg
                    , hfbPun = pun }))
#endif
      | FieldOcc _ (L _ nm) <- fieldOcc
      , Unqual nm' <- nm
      , not pun
      = Just (occNameFS nm', arg)

      | otherwise
      = Nothing

debugMsg :: LHsExpr GhcPs -> SDoc
debugMsg expr = pprSetDepth AllTheWay $ vcat [
      text "large-records: splicing in the following expression:"
    , ppr expr
    ]

{-------------------------------------------------------------------------------
  Main translation
-------------------------------------------------------------------------------}

anonRec ::
     Options
  -> LargeAnonNames
  -> SrcSpan
  -> [(FastString, LHsExpr GhcPs)]
  -> FreshT Hsc (LHsExpr GhcPs)
anonRec Options{noapply} names@LargeAnonNames{..} l = \fields ->
    applyDiff =<< go fields
  where
    go :: [(FastString, LHsExpr GhcPs)] -> FreshT Hsc (LHsExpr GhcPs)
    go fields
      | null fields = do
          return $ mkVar l largeAnon_empty
      | otherwise = do
          lift $ recordWithoutTypelet names l fields

    applyDiff :: LHsExpr GhcPs -> FreshT Hsc (LHsExpr GhcPs)
    applyDiff e
      | noapply   = return e
      | otherwise = return $ mkVar l largeAnon_applyPending `mkHsApp` e

-- | No type-level sharing
--
-- Support for @typelet@ was dropped in 0.4.0.
recordWithoutTypelet ::
     LargeAnonNames
  -> SrcSpan
  -> [(FastString, LHsExpr GhcPs)]
  -> Hsc (LHsExpr GhcPs)
recordWithoutTypelet LargeAnonNames{..} l = \fields -> do
    return $ go fields
  where
    go :: [(FastString, LHsExpr GhcPs)] -> LHsExpr GhcPs
    go []         = mkVar l largeAnon_empty
    go ((n,e):fs) = mkVar l largeAnon_insert `mkHsApps` [mkLabel l n, e, go fs]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mkVar :: SrcSpan -> RdrName -> LHsExpr GhcPs
mkVar l name = reLocA $ L l $ HsVar defExt (reLocA $ L l name)
