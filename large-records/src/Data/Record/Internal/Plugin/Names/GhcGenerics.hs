-- | Names for GHC generics
--
-- We cannot make this part of the main ghcGenerics, because the names clash
-- with @large-records@ generics.
module Data.Record.Internal.Plugin.Names.GhcGenerics (
    type_Generic
  , unq_type_Rep
  , unq_from
  , unq_to
  ) where

import Data.Record.Internal.GHC.Shim

{-------------------------------------------------------------------------------
  Names of GHC.Generics
-------------------------------------------------------------------------------}

type_Generic :: LRdrName
type_Generic = nameQT "Generic"

unq_type_Rep :: LRdrName
unq_type_Rep = nameUT "Rep"

unq_from, unq_to :: LRdrName
unq_from = nameUV "from"
unq_to   = nameUV "to"

{-------------------------------------------------------------------------------
  Internal auxiliary

  NOTE: Unqualified names are used when generating class instances.
-------------------------------------------------------------------------------}

ghcGenerics :: ModuleName
ghcGenerics = mkModuleName "GHC.Generics"

nameQT :: String -> LRdrName
nameQT var = noLoc $ mkRdrQual ghcGenerics $ mkTcOcc   var

nameUV, nameUT :: String -> LRdrName
nameUV var = noLoc $ mkRdrUnqual $ mkVarOcc var
nameUT var = noLoc $ mkRdrUnqual $ mkTcOcc  var
