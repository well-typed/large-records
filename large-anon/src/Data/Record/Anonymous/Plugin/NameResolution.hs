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
    , clsRecordConstraints   :: Class
    , clsRecordMetadata      :: Class
    , dataConDict            :: DataCon
    , dataConFieldLazy       :: DataCon
    , dataConFieldMetadata   :: DataCon
    , dataConMetadata        :: DataCon
    , dataConProxy           :: DataCon
    , idUnsafeCoerce         :: Id
    , idUnsafeDictRecord     :: Id
    , idUnsafeFieldMetadata  :: Id
    , idUnsafeRecordHasField :: Id
    , tyConDict              :: TyCon
    , tyConFieldMetadata     :: TyCon
    , tyConRecord            :: TyCon
    , tyConMerge             :: TyCon
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
    clsRecordConstraints <- getClass drai "RecordConstraints"
    clsRecordMetadata    <- getClass drai "RecordMetadata"

    dataConDict          <- getDataCon dsd "Dict"
    dataConFieldMetadata <- getDataCon drg "FieldMetadata"
    dataConFieldLazy     <- getDataCon drg "FieldLazy"
    dataConMetadata      <- getDataCon drg "Metadata"
    dataConProxy         <- getDataCon dp  "Proxy"

    idUnsafeCoerce         <- getVar uc   "unsafeCoerce"
    idUnsafeFieldMetadata  <- getVar drai "unsafeFieldMetadata"
    idUnsafeRecordHasField <- getVar drai "unsafeRecordHasField"
    idUnsafeDictRecord     <- getVar drai "unsafeDictRecord"

    tyConDict          <- getTyCon dsd  "Dict"
    tyConFieldMetadata <- getTyCon drg  "FieldMetadata"
    tyConRecord        <- getTyCon drai "Record"
    tyConMerge         <- getTyCon drai "Merge"

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