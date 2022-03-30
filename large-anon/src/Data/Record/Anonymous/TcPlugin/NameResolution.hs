{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.TcPlugin.NameResolution (
    ResolvedNames(..)
  , nameResolution
  ) where

import Data.Record.Anonymous.TcPlugin.GhcTcPluginAPI

-- | Names we need to parse constraints or generate core
--
-- Listed alphabetically.
data ResolvedNames = ResolvedNames {
      clsAllFields             :: Class
    , clsKnownFields           :: Class
    , clsKnownHash             :: Class
    , clsProject               :: Class
    , clsRecordHasField        :: Class
    , dataConDictAny           :: DataCon
    , idEvidenceAllFields      :: Id
    , idEvidenceKnownFields    :: Id
    , idEvidenceKnownHash      :: Id
    , idEvidenceProject        :: Id
    , idEvidenceRecordHasField :: Id
    , idUnsafeCoerce           :: Id
    , tyConDictAny             :: TyCon
    , tyConMerge               :: TyCon
    , tyConFieldTypes          :: TyCon
    , tyConPair                :: TyCon
    , tyConSimpleFieldTypes    :: TyCon
    }

nameResolution :: TcPluginM 'Init ResolvedNames
nameResolution = do
    modl <- getModule "large-anon" "Data.Record.Anon.Plugin.Internal.Runtime"

    let getClass       :: MonadTcPlugin m => String -> m Class
        getTyCon       :: MonadTcPlugin m => String -> m TyCon
        getDataCon     :: MonadTcPlugin m => String -> m DataCon
        getVar         :: MonadTcPlugin m => String -> m Id
        getPromDataCon :: MonadTcPlugin m => String -> m TyCon

        getClass       cls = lookupOrig modl (mkTcOcc cls)   >>= tcLookupClass
        getTyCon       con = lookupOrig modl (mkTcOcc con)   >>= tcLookupTyCon
        getDataCon     con = lookupOrig modl (mkDataOcc con) >>= tcLookupDataCon
        getVar         var = lookupOrig modl (mkVarOcc var)  >>= tcLookupId
        getPromDataCon con = promoteDataCon <$> getDataCon con

    clsAllFields             <- getClass "AllFields"
    clsKnownFields           <- getClass "KnownFields"
    clsKnownHash             <- getClass "KnownHash"
    clsProject               <- getClass "Project"
    clsRecordHasField        <- getClass "RecordHasField"

    dataConDictAny           <- getDataCon "DictAny"

    idEvidenceAllFields      <- getVar "evidenceAllFields"
    idEvidenceKnownFields    <- getVar "evidenceKnownFields"
    idEvidenceKnownHash      <- getVar "evidenceKnownHash"
    idEvidenceProject        <- getVar "evidenceProject"
    idEvidenceRecordHasField <- getVar "evidenceRecordHasField"
    idUnsafeCoerce           <- getVar "noInlineUnsafeCo"

    tyConDictAny             <- getTyCon       "DictAny"
    tyConFieldTypes          <- getTyCon       "FieldTypes"
    tyConMerge               <- getTyCon       "Merge"
    tyConPair                <- getPromDataCon ":="
    tyConSimpleFieldTypes    <- getTyCon       "SimpleFieldTypes"

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
