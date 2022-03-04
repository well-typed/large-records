{-# LANGUAGE CPP #-}

-- | Thin compatibility layer around GHC
--
-- This should be the only module with GHC-specific CPP directives, and the
-- rest of the plugin should not import from any GHC modules directly.
module Data.Record.Plugin.GHC.Shim (
    noExtField
  , qimportD
  ) where

import BasicTypes
import GHC

#if __GLASGOW_HASKELL__ < 810
noExtField :: NoExt
noExtField = noExt
#endif

-- | Construct @import qualified@ declaration
qimportD :: ModuleName -> LImportDecl GhcPs
qimportD name = noLoc $ ImportDecl {
      ideclExt       = noExtField
    , ideclSourceSrc = NoSourceText
    , ideclName      = noLoc name
    , ideclPkgQual   = Nothing
    , ideclSource    = False
    , ideclSafe      = False
    , ideclImplicit  = False
    , ideclAs        = Nothing
    , ideclHiding    = Nothing
#if __GLASGOW_HASKELL__ < 810
    , ideclQualified = True
#else
    , ideclQualified = QualifiedPre
#endif
    }

