-- | Names for GHC generics
--
-- We cannot make this part of the main ghcGenerics, because the names clash
-- with @large-records@ generics.
module Data.Record.Internal.Plugin.Names.GhcGenerics (
    type_Generic
  , unq_type_Rep
  , unq_from
  , unq_to
  , con_NoSourceUnpackedness
  , con_SourceNoUnpack
  , con_SourceUnpack
  , con_NoSourceStrictness
  , con_SourceLazy
  , con_SourceStrict
  , con_DecidedLazy
  , con_DecidedStrict
  , con_DecidedUnpack
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

con_NoSourceUnpackedness :: LRdrName
con_SourceNoUnpack       :: LRdrName
con_SourceUnpack         :: LRdrName
con_NoSourceStrictness   :: LRdrName
con_SourceLazy           :: LRdrName
con_SourceStrict         :: LRdrName
con_DecidedLazy          :: LRdrName
con_DecidedStrict        :: LRdrName
con_DecidedUnpack        :: LRdrName

con_NoSourceUnpackedness  = nameQC "NoSourceUnpackedness"
con_SourceNoUnpack        = nameQC "SourceNoUnpack"
con_SourceUnpack          = nameQC "SourceUnpack"
con_NoSourceStrictness    = nameQC "NoSourceStrictness"
con_SourceLazy            = nameQC "SourceLazy"
con_SourceStrict          = nameQC "SourceStrict"
con_DecidedLazy           = nameQC "DecidedLazy"
con_DecidedStrict         = nameQC "DecidedStrict"
con_DecidedUnpack         = nameQC "DecidedUnpack"

{-------------------------------------------------------------------------------
  Internal auxiliary

  NOTE: Unqualified names are used when generating class instances.
-------------------------------------------------------------------------------}

ghcGenerics :: ModuleName
ghcGenerics = mkModuleName "GHC.Generics"

nameQT, nameQC :: String -> LRdrName
nameQT var = noLoc $ mkRdrQual ghcGenerics $ mkTcOcc   var
nameQC var = noLoc $ mkRdrQual ghcGenerics $ mkDataOcc var

nameUV, nameUT :: String -> LRdrName
nameUV var = noLoc $ mkRdrUnqual $ mkVarOcc var
nameUT var = noLoc $ mkRdrUnqual $ mkTcOcc  var
