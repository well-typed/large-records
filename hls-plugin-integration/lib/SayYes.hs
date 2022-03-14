{-# LANGUAGE CPP #-}

module SayYes (plugin) where

import Control.Monad
import Data.Data
import Data.Generics
import Language.Haskell.TH (Extension(..))

#if __GLASGOW_HASKELL__ < 900
import Bag
import ErrUtils
import GHC
import GhcPlugins
#else
import GHC.Data.Bag
import GHC.Hs
import GHC.Plugins
import GHC.Utils.Error
#endif

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = \_ _ -> action }

action :: HsParsedModule -> Hsc HsParsedModule
action parsed@HsParsedModule{hpm_module = m} =
    (\m' -> parsed {hpm_module = m'}) <$>
      gmapM (everywhereM (mkM sayYes)) m

sayYes :: HsExpr GhcPs -> Hsc (HsExpr GhcPs)
sayYes e = do
   case e of
     HsVar _ (L l x) | occNameString (rdrNameOcc x) == "False" -> do
       dynFlags <- getDynFlags
       unless (ScopedTypeVariables `xopt` dynFlags) $
         issueError l $ text "Everyone needs scoped type variables"
       return $ e
     _otherwise  ->
       return $ e

issueError :: SrcSpan -> SDoc -> Hsc a
issueError l errMsg = do
    dynFlags <- getDynFlags
    throwOneError $
      mkErrMsg dynFlags l neverQualify errMsg

issueWarning :: SrcSpan -> SDoc -> Hsc ()
issueWarning l errMsg = do
    dynFlags <- getDynFlags
    liftIO $ printOrThrowWarnings dynFlags . listToBag . (:[]) $
      mkWarnMsg dynFlags l neverQualify errMsg
