{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Data.Record.Anon.Internal.Plugin.Source (sourcePlugin) where

import Control.Monad
import Control.Monad.Trans
import Data.Generics (everywhereM, mkM)

import Data.Record.Anon.Internal.Plugin.Source.GhcShim
import Data.Record.Anon.Internal.Plugin.Source.Names
import Data.Record.Anon.Internal.Plugin.Source.NamingT
import Data.Record.Anon.Internal.Plugin.Source.Options

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

sourcePlugin :: [String] -> HsParsedModule -> Hsc HsParsedModule
sourcePlugin opts
             parsed@HsParsedModule{
                 hpm_module = L l modl@HsModule{
                     hsmodDecls   = decls
                   , hsmodImports = imports
                   }
               } = do
    (decls', modls) <- runNamingHsc $
                         everywhereM
                           (mkM $ transformExpr $ parseOpts opts)
                           decls
    return $ parsed {
        hpm_module = L l $ modl {
            hsmodDecls   = decls'
          , hsmodImports = imports ++ map (importDecl True) modls
          }
      }

transformExpr :: Options -> LHsExpr GhcPs -> NamingT Hsc (LHsExpr GhcPs)
transformExpr options@Options{debug} e@(L l expr)
  | RecordCon _ext (L _ nm) (HsRecFields flds dotdot) <- expr
  , Unqual nm' <- nm
  , Nothing    <- dotdot
  , Just mode  <- parseMode (occNameString nm')
  , Just flds' <- mapM getField flds
  = do e' <- anonRec options mode l flds'
       when debug $ lift $ issueWarning l (debugMsg e')
       return e'

  | otherwise
  = return e
  where
    getField ::
         LHsRecField GhcPs (LHsExpr GhcPs)
      -> Maybe (FastString, LHsExpr GhcPs)
    getField (L _ (HsRecField (L _ fieldOcc) arg pun))
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
  -> Mode
  -> SrcSpan
  -> [(FastString, LHsExpr GhcPs)]
  -> NamingT Hsc (LHsExpr GhcPs)
anonRec Options{typelet, noapply} mode l = \fields ->
    applyDiff =<< go fields
  where
    LargeAnonNames{..} = largeAnonNames mode

    go :: [(FastString, LHsExpr GhcPs)] -> NamingT Hsc (LHsExpr GhcPs)
    go fields
      | null fields = do
          useName largeAnon_empty
          return $ mkVar l largeAnon_empty
      | not typelet = do
          recordWithoutTypelet mode l fields
      | otherwise = do
          p       <- freshVar l "p"
          fields' <- mapM (\(n, e) -> (n,e,) <$> freshVar l "xs") fields
          recordWithTypelet mode l p fields'

    applyDiff :: LHsExpr GhcPs -> NamingT Hsc (LHsExpr GhcPs)
    applyDiff e
      | noapply   = return e
      | otherwise = do
          useName largeAnon_applyPending
          return $ mkVar l largeAnon_applyPending `mkHsApp` e

recordWithoutTypelet ::
     Mode
  -> SrcSpan
  -> [(FastString, LHsExpr GhcPs)]
  -> NamingT Hsc (LHsExpr GhcPs)
recordWithoutTypelet mode l = \fields -> do
    useName largeAnon_empty
    useName largeAnon_insert
    return $ go fields
  where
    LargeAnonNames{..} = largeAnonNames mode

    go :: [(FastString, LHsExpr GhcPs)] -> LHsExpr GhcPs
    go []         = mkVar l largeAnon_empty
    go ((n,e):fs) = mkVar l largeAnon_insert `mkHsApps` [mkLabel l n, e, go fs]

-- | Experimental support for typelet
--
-- See documentation of 'letRecordT' and 'letInsertAs'.
recordWithTypelet ::
     Mode
  -> SrcSpan
  -> RdrName                                -- ^ Fresh var for the proxy
  -> [(FastString, LHsExpr GhcPs, RdrName)] -- ^ Fresh var for each insert
  -> NamingT Hsc (LHsExpr GhcPs)
recordWithTypelet mode l p = \fields -> do
    useName largeAnon_empty
    useName largeAnon_insert
    useName largeAnon_letRecordT
    useName largeAnon_letInsertAs
    useName typelet_castEqual

    return $
      mkHsApp (mkVar l largeAnon_letRecordT) $
        simpleLam p $ mkHsApp (mkVar l typelet_castEqual) $
          go (mkVar l largeAnon_empty) $ reverse fields
  where
    LargeAnonNames{..} = largeAnonNames mode

    go ::
         LHsExpr GhcPs
      -> [(FastString, LHsExpr GhcPs, RdrName)]
      -> LHsExpr GhcPs
    go prev []           = mkHsApp  (mkVar l typelet_castEqual) prev
    go prev ((n,e,x):fs) = mkHsApps (mkVar l largeAnon_letInsertAs) [
          mkVar l p
        , mkLabel l n
        , e
        , prev
        , simpleLam x $ go (mkVar l x) fs
        ]
      where

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mkVar :: SrcSpan -> RdrName -> LHsExpr GhcPs
mkVar l name = L l $ HsVar defExt (L l name)

mkLabel :: SrcSpan -> FastString -> LHsExpr GhcPs
mkLabel l n = L l $ HsOverLabel defExt Nothing n

-- | Construct simple lambda
--
-- Constructs lambda of the form
--
-- > \x -> e
simpleLam :: RdrName -> LHsExpr GhcPs -> LHsExpr GhcPs
simpleLam x body = mkHsLam [nlVarPat x] body
