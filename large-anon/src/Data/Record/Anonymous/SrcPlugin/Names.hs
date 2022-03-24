-- | Names used in code generation
--
-- Intended for qualified import.
--
-- > import Data.Record.Anonymous.SrcPlugin.Names (Names)
-- > import qualified Data.Record.Anonymous.SrcPlugin.Names as N
module Data.Record.Anonymous.SrcPlugin.Names (
    Names(..)
  , advanced
  , simple
  ) where

import GHC
import GhcPlugins

-- | Named required for code generation
--
-- All names are expected to be qualified with the full module name
data Names = Names {
      nameEmpty  :: RdrName
    , nameInsert :: RdrName
    }

-- | Names used in the advanced interface
advanced :: Names
advanced = Names {
      nameEmpty  = mkRdrQual modl $ mkVarOcc "empty"
    , nameInsert = mkRdrQual modl $ mkVarOcc "insert"
    }
  where
    modl :: ModuleName
    modl = mkModuleName "Data.Record.Anonymous.Advanced"

-- | Names used in the simple interface
simple :: Names
simple = Names {
      nameEmpty  = mkRdrQual modl $ mkVarOcc "empty"
    , nameInsert = mkRdrQual modl $ mkVarOcc "insert"
    }
  where
    modl :: ModuleName
    modl = mkModuleName "Data.Record.Anonymous.Simple"



