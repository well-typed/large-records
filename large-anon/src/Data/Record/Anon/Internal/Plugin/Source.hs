{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

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
  | RecordCon _ext (L _ nm) (HsRecFields flds dotdot) <- expr
  , Unqual nm' <- nm
  , Nothing    <- dotdot
  , Just mode  <- parseMode (occNameString nm')
  , Just flds' <- mapM getField flds
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
    getField (L _ (HsRecField
                    { hsRecFieldLbl = L _ fieldOcc
                    , hsRecFieldArg = arg
                    , hsRecPun = pun }))
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
anonRec Options{typelet, noapply} names@LargeAnonNames{..} l = \fields ->
    applyDiff =<< go fields
  where
    go :: [(FastString, LHsExpr GhcPs)] -> FreshT Hsc (LHsExpr GhcPs)
    go fields
      | null fields = do
          return $ mkVar l largeAnon_empty
      | not typelet = do
          lift $ recordWithoutTypelet names l fields
      | otherwise = do
          p       <- freshVar l "p"
          fields' <- mapM (\(n, e) -> (n,e,) <$> freshVar l "xs") fields
          lift $ recordWithTypelet names l p fields'

    applyDiff :: LHsExpr GhcPs -> FreshT Hsc (LHsExpr GhcPs)
    applyDiff e
      | noapply   = return e
      | otherwise = return $ mkVar l largeAnon_applyPending `mkHsApp` e

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

-- | Experimental support for typelet
--
-- See documentation of 'letRecordT' and 'letInsertAs'.
recordWithTypelet ::
     LargeAnonNames
  -> SrcSpan
  -> RdrName                                -- ^ Fresh var for the proxy
  -> [(FastString, LHsExpr GhcPs, RdrName)] -- ^ Fresh var for each insert
  -> Hsc (LHsExpr GhcPs)
recordWithTypelet LargeAnonNames{..} l p = \fields -> do
    return $
      mkHsApp (mkVar l largeAnon_letRecordT) $
        simpleLam p $ mkHsApp (mkVar l typelet_castEqual) $
          go (mkVar l largeAnon_empty) $ reverse fields
  where
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
mkVar l name = reLocA $ L l $ HsVar defExt (reLocA $ L l name)

-- | Construct simple lambda
--
-- Constructs lambda of the form
--
-- > \x -> e
simpleLam :: RdrName -> LHsExpr GhcPs -> LHsExpr GhcPs
simpleLam x body = mkHsLam [nlVarPat x] body
