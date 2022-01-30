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
    , clsIsomorphic          :: Class
    , clsKnownSymbol         :: Class
    , clsKnownFieldLabel     :: Class
    , clsRecordConstraints   :: Class -- TODO: remove
    , clsRecordDicts         :: Class
    , clsRecordMetadata      :: Class
    , dataConDict            :: DataCon
    , dataConFieldLazy       :: DataCon
    , dataConFieldMetadata'  :: DataCon
    , dataConProxy           :: DataCon
    , idUnsafeCoerce         :: Id
    , idUnsafeDictRecord     :: Id
    , idUnsafeRecordDicts    :: Id
    , idUnsafeRecordHasField :: Id
    , idUnsafeRecordMetadata :: Id
    , tyConDict              :: TyCon
    , tyConFieldMetadata'    :: TyCon
    , tyConMerge             :: TyCon
    , tyConRecord            :: TyCon
    , tyConRecordMetadataOf  :: TyCon
    }

nameResolution :: TcPluginM 'Init ResolvedNames
nameResolution = do

    dp   <- getModule "base"            "Data.Proxy"
    drai <- getModule "large-anon"      "Data.Record.Anonymous.Internal"
    drg  <- getModule "large-generics"  "Data.Record.Generic"
    dsd  <- getModule "sop-core"        "Data.SOP.Dict"
    grc  <- getModule "record-hasfield" "GHC.Records.Compat"
    uc   <- getModule "base"            "Unsafe.Coerce"

    clsHasField          <- getClass grc  "HasField"
    clsIsomorphic        <- getClass drai "Isomorphic"
    clsKnownSymbol       <- tcLookupClass knownSymbolClassName
    clsKnownFieldLabel   <- getClass drai "KnownFieldLabel"
    clsRecordConstraints <- getClass drai "RecordConstraints"
    clsRecordDicts       <- getClass drai "RecordDicts"
    clsRecordMetadata    <- getClass drai "RecordMetadata"

    dataConDict           <- getDataCon dsd  "Dict"
    dataConFieldLazy      <- getDataCon drg  "FieldLazy"
    dataConFieldMetadata' <- getDataCon drai "FieldMetadata'"
    dataConProxy          <- getDataCon dp   "Proxy"

    idUnsafeCoerce         <- getVar uc   "unsafeCoerce"
    idUnsafeDictRecord     <- getVar drai "unsafeDictRecord"
    idUnsafeRecordDicts    <- getVar drai "unsafeRecordDicts"
    idUnsafeRecordHasField <- getVar drai "unsafeRecordHasField"
    idUnsafeRecordMetadata <- getVar drai "unsafeRecordMetadata"

    tyConDict             <- getTyCon dsd  "Dict"
    tyConFieldMetadata'   <- getTyCon drai "FieldMetadata'"
    tyConMerge            <- getTyCon drai "Merge"
    tyConRecord           <- getTyCon drai "Record"
    tyConRecordMetadataOf <- getTyCon drai "RecordMetadataOf"

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