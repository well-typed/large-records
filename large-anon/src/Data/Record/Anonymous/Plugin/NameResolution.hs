{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.NameResolution (
    ResolvedNames(..)
  , nameResolution
  ) where

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI

-- | Names we need to parse constraints or generate core
--
-- Listed alphabetically.
data ResolvedNames = ResolvedNames {
      clsHasField            :: Class
    , clsAllFields           :: Class
    , clsIsomorphic          :: Class
    , clsKnownFields         :: Class
    , clsKnownSymbol         :: Class
    , dataConDict            :: DataCon
    , dataConFieldLazy       :: DataCon
    , dataConFieldMetadata   :: DataCon
    , dataConProxy           :: DataCon
    , idEvidenceHasField     :: Id
    , idEvidenceAllFields    :: Id
    , idEvidenceKnownFields  :: Id
    , idUnsafeCoerce         :: Id
    , tyConDict              :: TyCon
    , tyConFieldMetadata     :: TyCon
    , tyConMerge             :: TyCon
    , tyConRecord            :: TyCon
    , tyConFieldTypes        :: TyCon
    }

nameResolution :: TcPluginM 'Init ResolvedNames
nameResolution = do

    dDict        <- getModule "sop-core"        "Data.SOP.Dict"
    dProxy       <- getModule "base"            "Data.Proxy"
    draiEvidence <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Evidence"
    draiRecord   <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Record"
    draiRow      <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Row"
    drGeneric    <- getModule "large-generics"  "Data.Record.Generic"
    grCompat     <- getModule "record-hasfield" "GHC.Records.Compat"
    uCoerce      <- getModule "base"            "Unsafe.Coerce"

    clsAllFields    <- getClass draiRow  "AllFields"
    clsHasField     <- getClass grCompat "HasField"
    clsIsomorphic   <- getClass draiRow  "Isomorphic"
    clsKnownFields  <- getClass draiRow  "KnownFields"
    clsKnownSymbol  <- tcLookupClass knownSymbolClassName

    dataConDict          <- getDataCon dDict     "Dict"
    dataConFieldLazy     <- getDataCon drGeneric "FieldLazy"
    dataConFieldMetadata <- getDataCon drGeneric "FieldMetadata"
    dataConProxy         <- getDataCon dProxy    "Proxy"

    idEvidenceHasField    <- getVar draiEvidence "evidenceHasField"
    idEvidenceAllFields   <- getVar draiEvidence "evidenceAllFields"
    idEvidenceKnownFields <- getVar draiEvidence "evidenceKnownFields"
    idUnsafeCoerce        <- getVar uCoerce      "unsafeCoerce"

    tyConDict          <- getTyCon dDict      "Dict"
    tyConFieldMetadata <- getTyCon drGeneric  "FieldMetadata"
    tyConMerge         <- getTyCon draiRow    "Merge"
    tyConRecord        <- getTyCon draiRecord "Record"
    tyConFieldTypes    <- getTyCon draiRow    "FieldTypes"

    return $ ResolvedNames {..}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getModule :: MonadTcPlugin m => String -> String -> m Module
getModule pkg modl = do
    r <- findImportedModule (mkModuleName modl) (Just (fsLit pkg))
    case r of
      Found _ m  -> return m
      _otherwise -> panic $ "Could not find " ++ modl ++ " in package " ++ pkg

getClass :: MonadTcPlugin m => Module -> String -> m Class
getClass modl cls = lookupOrig modl (mkTcOcc cls) >>= tcLookupClass

getTyCon :: MonadTcPlugin m => Module -> String -> m TyCon
getTyCon modl con = lookupOrig modl (mkTcOcc con) >>= tcLookupTyCon

getDataCon :: MonadTcPlugin m => Module -> String -> m DataCon
getDataCon modl con = lookupOrig modl (mkDataOcc con) >>= tcLookupDataCon

getVar :: MonadTcPlugin m => Module -> String -> m Id
getVar modl var = lookupOrig modl (mkVarOcc var) >>= tcLookupId