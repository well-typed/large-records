{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anon.Internal.Plugin.TC.NameResolution (
    ResolvedNames(..)
  , nameResolution
  ) where

import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI

-- | Names we need to parse constraints or generate core
--
-- Listed alphabetically.
data ResolvedNames = ResolvedNames {
      clsAllFields          :: Class
    , clsKnownFields        :: Class
    , clsKnownHash          :: Class
    , clsRowHasField        :: Class
    , clsSubRow             :: Class
    , dataConDictAny        :: DataCon
    , idEvidenceAllFields   :: Id
    , idEvidenceKnownFields :: Id
    , idEvidenceKnownHash   :: Id
    , idEvidenceRowHasField :: Id
    , idEvidenceSubRow      :: Id
    , idUnsafeCoerce        :: Id
    , tyConDictAny          :: TyCon
    , tyConMerge            :: TyCon
    , tyConFieldTypes       :: TyCon
    , tyConPair             :: TyCon
    , tyConSimpleFieldTypes :: TyCon
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

    clsAllFields          <- getClass "AllFields"
    clsKnownFields        <- getClass "KnownFields"
    clsKnownHash          <- getClass "KnownHash"
    clsRowHasField        <- getClass "RowHasField"
    clsSubRow             <- getClass "SubRow"

    dataConDictAny        <- getDataCon "DictAny"

    idEvidenceAllFields   <- getVar "evidenceAllFields"
    idEvidenceKnownFields <- getVar "evidenceKnownFields"
    idEvidenceKnownHash   <- getVar "evidenceKnownHash"
    idEvidenceRowHasField <- getVar "evidenceRowHasField"
    idEvidenceSubRow      <- getVar "evidenceSubRow"
    idUnsafeCoerce        <- getVar "noInlineUnsafeCo"

    tyConDictAny          <- getTyCon       "DictAny"
    tyConFieldTypes       <- getTyCon       "FieldTypes"
    tyConMerge            <- getTyCon       "Merge"
    tyConPair             <- getPromDataCon ":="
    tyConSimpleFieldTypes <- getTyCon       "SimpleFieldTypes"

    return $ ResolvedNames {..}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getModule :: MonadTcPlugin m => String -> String -> m Module
getModule pkg modl = do
    r <- findImportedModule (mkModuleName modl) (OtherPkg $ stringToUnitId pkg)
    case r of
      Found _ m  -> return m
      _otherwise -> panic $ "Could not find " ++ modl ++ " in package " ++ pkg
