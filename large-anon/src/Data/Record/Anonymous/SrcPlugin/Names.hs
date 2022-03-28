-- | Names used in code generation
--
-- Intended for unqualified.
module Data.Record.Anonymous.SrcPlugin.Names (
    -- * large-anon
    LargeAnonNames(..)
  , largeAnonNames
    -- * typelet
  , typelet_castEqual
  ) where

import Data.Record.Anonymous.SrcPlugin.GhcShim
import Data.Record.Anonymous.SrcPlugin.Options (Mode(..))

{-------------------------------------------------------------------------------
  large-anon
-------------------------------------------------------------------------------}

-- | Named required for code generation
--
-- All names are expected to be qualified with the full module name
data LargeAnonNames = LargeAnonNames {
      largeAnon_empty       :: RdrName
    , largeAnon_insert      :: RdrName
    , largeAnon_letRecordT  :: RdrName
    , largeAnon_letInsertAs :: RdrName
    }

largeAnonNames :: Mode -> LargeAnonNames
largeAnonNames mode = LargeAnonNames {
      largeAnon_empty       = mkRdrQual modl $ mkVarOcc "empty"
    , largeAnon_insert      = mkRdrQual modl $ mkVarOcc "insert"
    , largeAnon_letRecordT  = mkRdrQual modl $ mkVarOcc "letRecordT"
    , largeAnon_letInsertAs = mkRdrQual modl $ mkVarOcc "letInsertAs"
    }
  where
    modl :: ModuleName
    modl = case mode of
             Simple   -> mkModuleName "Data.Record.Anonymous.Simple"
             Advanced -> mkModuleName "Data.Record.Anonymous.Advanced"

{-------------------------------------------------------------------------------
  Typelet
-------------------------------------------------------------------------------}

typelet :: ModuleName
typelet = mkModuleName "TypeLet"

typelet_castEqual :: RdrName
typelet_castEqual = mkRdrQual typelet $ mkVarOcc "castEqual"
