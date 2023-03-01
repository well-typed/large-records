module TypeLet.Plugin.NameResolution (
    ResolvedNames(..)
  , resolveNames
  ) where

import TypeLet.Plugin.GhcTcPluginAPI

data ResolvedNames = ResolvedNames {
      clsEqual :: Class
    , clsLet   :: Class
    }

instance Outputable ResolvedNames where
  ppr ResolvedNames{..} = vcat [
        text "ResolvedNames {"
      , nest 2 $ vcat [
            text "clsEqual =" <+> ppr clsEqual
          , text "clsLet   =" <+> ppr clsLet
          ]
      , text "}"
      ]

resolveNames :: TcPluginM 'Init ResolvedNames
resolveNames = do
    pkgQual <- resolveImport typeletMod (Just $ fsLit "typelet")
    modl    <- do res <- findImportedModule typeletMod pkgQual
                  case res of
                    Found _ m  -> return m
                    _otherwise -> panic $ "resolveNames: could not find "
                                       ++ showSDocUnsafe (ppr typeletMod)

    -- Constraints handled by the plugin

    clsEqual <- tcLookupClass =<< lookupOrig modl (mkTcOcc "Equal")
    clsLet   <- tcLookupClass =<< lookupOrig modl (mkTcOcc "Let")
    return ResolvedNames{..}
  where
    typeletMod :: ModuleName
    typeletMod = mkModuleName "TypeLet.UserAPI"
