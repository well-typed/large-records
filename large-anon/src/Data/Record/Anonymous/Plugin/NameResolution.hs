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
      clsAllFields          :: Class
    , clsHasField           :: Class
    , clsKnownFields        :: Class
    , clsKnownHash          :: Class
    , clsKnownSymbol        :: Class
    , clsProject            :: Class
    , dataConDict           :: DataCon
    , dataConFieldStrict    :: DataCon
    , dataConFieldName      :: DataCon
    , dataConFieldMetadata  :: DataCon
    , dataConProxy          :: DataCon
    , idEvidenceAllFields   :: Id
    , idEvidenceHasField    :: Id
    , idEvidenceKnownFields :: Id
    , idEvidenceKnownHash   :: Id
    , idEvidenceProject     :: Id
    , idUnsafeCoerce        :: Id
    , tyConDict             :: TyCon
    , tyConFieldMetadata    :: TyCon
    , tyConMerge            :: TyCon
    , tyConRecord           :: TyCon
    , tyConFieldTypes       :: TyCon
    , tyConPair             :: TyCon
    , tyConSimpleFieldTypes :: TyCon
    }

nameResolution :: TcPluginM 'Init ResolvedNames
nameResolution = do

    -- TODO: Introduce a single module for the plugin to import from

    dDict        <- getModule "sop-core"        "Data.SOP.Dict"
    dProxy       <- getModule "base"            "Data.Proxy"
    draiEvidence <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Evidence"
    draiRecord   <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Record"
    draiRow      <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Row"
    draiField    <- getModule "large-anon"      "Data.Record.Anonymous.Internal.Row.FieldName"
    drGeneric    <- getModule "large-generics"  "Data.Record.Generic"
    grCompat     <- getModule "record-hasfield" "GHC.Records.Compat"
    uCoerce      <- getModule "base"            "Unsafe.Coerce"

    clsAllFields   <- getClass draiRow  "AllFields"
    clsHasField    <- getClass grCompat "HasField"
    clsKnownFields <- getClass draiRow  "KnownFields"
    clsKnownHash   <- getClass draiField "KnownHash"
    clsKnownSymbol <- tcLookupClass knownSymbolClassName
    clsProject     <- getClass draiRow  "Project"

    dataConDict          <- getDataCon dDict     "Dict"
    dataConFieldMetadata <- getDataCon drGeneric "FieldMetadata"
    dataConFieldName     <- getDataCon draiField "FieldName"
    dataConFieldStrict   <- getDataCon drGeneric "FieldStrict"
    dataConProxy         <- getDataCon dProxy    "Proxy"

    idEvidenceAllFields   <- getVar draiEvidence "evidenceAllFields"
    idEvidenceHasField    <- getVar draiEvidence "evidenceHasField"
    idEvidenceKnownFields <- getVar draiEvidence "evidenceKnownFields"
    idEvidenceKnownHash   <- getVar draiEvidence "evidenceKnownHash"
    idEvidenceProject     <- getVar draiEvidence "evidenceProject"
    idUnsafeCoerce        <- getVar uCoerce      "unsafeCoerce" -- TODO: use noInlineUnsafeCo instead

    tyConDict             <- getTyCon       dDict      "Dict"
    tyConFieldMetadata    <- getTyCon       drGeneric  "FieldMetadata"
    tyConFieldTypes       <- getTyCon       draiRow    "FieldTypes"
    tyConMerge            <- getTyCon       draiRow    "Merge"
    tyConPair             <- getPromDataCon draiRow    ":="
    tyConRecord           <- getTyCon       draiRecord "Record"
    tyConSimpleFieldTypes <- getTyCon       draiRow    "SimpleFieldTypes"

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

getPromDataCon :: MonadTcPlugin m => Module -> String -> m TyCon
getPromDataCon modl con = promoteDataCon <$> getDataCon modl con

getVar :: MonadTcPlugin m => Module -> String -> m Id
getVar modl var = lookupOrig modl (mkVarOcc var) >>= tcLookupId