{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Record.Anonymous.SrcPlugin (sourcePlugin) where

import Data.Bifunctor
import Data.Generics (everywhereM, mkM)
import Data.Set (Set)

import qualified Data.Set as Set

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT, runStateT, state)

import Data.Record.Anonymous.SrcPlugin.Names (Names)
import Data.Record.Anonymous.SrcPlugin.GhcShim

import qualified Data.Record.Anonymous.SrcPlugin.Names as N

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

sourcePlugin :: HsParsedModule -> Hsc HsParsedModule
sourcePlugin parsed@HsParsedModule{hpm_module = L l modl} = do
    modl' <- transformExprs modl
    return $ parsed { hpm_module = L l modl' }

{-------------------------------------------------------------------------------
  Module
-------------------------------------------------------------------------------}

data Mode = Simple | Advanced

transformExprs :: HsModule GhcPs -> Hsc (HsModule GhcPs)
transformExprs modl@HsModule{hsmodDecls = decls, hsmodImports = imports} = do
    (decls', modls) <- runTrackImports $ everywhereM (mkM goExpr) decls
    return modl {
        hsmodDecls   = decls'
      , hsmodImports = imports ++ map (importDecl True) modls
      }
  where
    goExpr :: LHsExpr GhcPs -> TrackImports Hsc (LHsExpr GhcPs)
    goExpr e@(L l expr)
      | RecordCon _ext (L _ nm) (HsRecFields flds dotdot) <- expr
      , Unqual nm' <- nm
      , Nothing    <- dotdot
      , Just mode  <- pickMode (occNameString nm')
      , Just flds' <- mapM getField flds
      = anonRec mode l flds'

      | otherwise
      = return e

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

    pickMode :: String -> Maybe Mode
    pickMode "ANON"   = Just Simple
    pickMode "ANON_F" = Just Advanced
    pickMode _        = Nothing

{-------------------------------------------------------------------------------
  Main translation
-------------------------------------------------------------------------------}

anonRec :: forall m.
     Monad m
  => Mode
  -> SrcSpan
  -> [(FastString, LHsExpr GhcPs)]
  -> TrackImports m (LHsExpr GhcPs)
anonRec mode = \l fields -> do
    varEmpty <- mkVar mode l N.nameEmpty
    fields'  <- mapM (uncurry insert) fields
    return $ foldr (.) id fields' varEmpty
  where
    insert ::
         FastString
      -> LHsExpr GhcPs
      -> TrackImports m (LHsExpr GhcPs -> LHsExpr GhcPs)
    insert nm expr@(L l _) = do
        varInsert <- mkVar mode l N.nameInsert
        return $ \r -> varInsert `mkHsApp` label `mkHsApp` expr `mkHsApp` r
      where
        label :: LHsExpr GhcPs
        label = L l $ HsOverLabel defExt Nothing nm

{-------------------------------------------------------------------------------
  Auxiliary: keep track of which imports we need
-------------------------------------------------------------------------------}

data Env = Env {
      envNamesAdvanced :: Names
    , envNamesSimple   :: Names
    }

newtype TrackImports m a = WrapTrackImports {
      unwrapTrackImports :: StateT (Set ModuleName) (ReaderT Env m) a
    }
  deriving (Functor, Applicative, Monad)

runTrackImports :: Functor m => TrackImports m a -> m (a, [ModuleName])
runTrackImports =
      fmap (second Set.toList)
    . flip runReaderT env
    . flip runStateT  Set.empty
    . unwrapTrackImports
  where
    env :: Env
    env = Env {
          envNamesAdvanced = N.advanced
        , envNamesSimple   = N.simple
        }

useName :: Monad m => Mode -> (Names -> RdrName) -> TrackImports m RdrName
useName mode f = WrapTrackImports $ do
    env <- ask
    let nm = case mode of
               Simple   -> f $ envNamesSimple   env
               Advanced -> f $ envNamesAdvanced env
    case nm of
      Qual modl _ -> state $ \imports -> (nm, Set.insert modl imports)
      _otherwise  -> error "getName: expected qualified name"

mkVar ::
     Monad m
  => Mode
  -> SrcSpan
  -> (Names -> RdrName)
  -> TrackImports m (LHsExpr GhcPs)
mkVar mode l f = aux <$> useName mode f
  where
    aux :: RdrName -> LHsExpr GhcPs
    aux name = L l (HsVar defExt (L l name))
