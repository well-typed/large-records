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
      clsHasField           :: Class
    , clsAllFields          :: Class
    , clsIsomorphic         :: Class
    , clsKnownFields        :: Class
    , clsKnownHash          :: Class
    , clsKnownSymbol        :: Class
    , dataConDict           :: DataCon
    , dataConFieldStrict    :: DataCon
    , dataConFieldName      :: DataCon
    , dataConFieldMetadata  :: DataCon
    , dataConProxy          :: DataCon
    , idEvidenceAllFields   :: Id
    , idEvidenceHasField    :: Id
    , idEvidenceIsomorphic  :: Id
    , idEvidenceKnownFields :: Id
    , idEvidenceKnownHash   :: Id
    , idUnsafeCoerce        :: Id
    , tyConDict             :: TyCon
    , tyConFieldMetadata    :: TyCon
    , tyConMerge            :: TyCon
    , tyConRecord           :: TyCon
    , tyConFieldTypes       :: TyCon
    }

nameResolution :: TcPluginM 'Init ResolvedNames
nameResolution = do

    -- TODO: Introduce a single module for the plugin to import from

    dDict        <- getModule "sop-core"        "Data.SOP.Dict"
    dProxy       <- getModule "base"            "Data.Proxy"
    draiEvidence <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Evidence"
    draiRecord   <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Record"
    draiRow      <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Row"
    draiField    <- getModule "large-anon"      "Data.Record.Anonymous.Internal.FieldName"
    drGeneric    <- getModule "large-generics"  "Data.Record.Generic"
    grCompat     <- getModule "record-hasfield" "GHC.Records.Compat"
    uCoerce      <- getModule "base"            "Unsafe.Coerce"

    clsAllFields   <- getClass draiRow  "AllFields"
    clsHasField    <- getClass grCompat "HasField"
    clsIsomorphic  <- getClass draiRow  "Isomorphic"
    clsKnownFields <- getClass draiRow  "KnownFields"
    clsKnownHash   <- getClass draiField "KnownHash"
    clsKnownSymbol <- tcLookupClass knownSymbolClassName

    dataConDict          <- getDataCon dDict     "Dict"
    dataConFieldMetadata <- getDataCon drGeneric "FieldMetadata"
    dataConFieldName     <- getDataCon draiField "FieldName"
    dataConFieldStrict   <- getDataCon drGeneric "FieldStrict"
    dataConProxy         <- getDataCon dProxy    "Proxy"

    idEvidenceAllFields   <- getVar draiEvidence "evidenceAllFields"
    idEvidenceHasField    <- getVar draiEvidence "evidenceHasField"
    idEvidenceIsomorphic  <- getVar draiEvidence "evidenceIsomorphic"
    idEvidenceKnownFields <- getVar draiEvidence "evidenceKnownFields"
    idEvidenceKnownHash   <- getVar draiEvidence "evidenceKnownHash"
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