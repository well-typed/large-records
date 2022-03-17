{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.KnownHash (
    CKnownHash(..)
  , parseKnownHash
  , solveKnownHash
  ) where

import Data.Hashable (hash)
import Data.Void

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.TyConSubst

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of an @KnownFieldLabel f@ constraint
data CKnownHash = CKnownHash {
      -- | The underlying @FastString@ when the label is a literal.
      knownHashLabel :: FastString

      -- | The raw type argument to the @KnownFieldLabel@ constraint.
    , knownHashType :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CKnownHash where
  ppr (CKnownHash hashLabel hashType) = parens $
      text "CKnownHash" <+> braces (vcat [
          text "knownHashLabel" <+> text "=" <+> ppr hashLabel
        , text "knownHashType"  <+> text "=" <+> ppr hashType
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseKnownHash ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CKnownHash)
parseKnownHash _ ResolvedNames{..} =
    parseConstraint' clsKnownHash $ \[ty] -> do
      label <- isStrLitTy ty
      return $ CKnownHash {
          knownHashLabel = label
        , knownHashType  = ty
        }

evidenceKnownFieldLabel ::
     ResolvedNames
  -> CKnownHash
  -> TcPluginM 'Solve EvTerm
evidenceKnownFieldLabel ResolvedNames{..} CKnownHash{..} =
    return $
      evDataConApp
        (classDataCon clsKnownHash)
        [knownHashType]
        [ mkCoreApps (Var idEvidenceKnownHash) [
              Type knownHashType
            , mkUncheckedIntExpr (fromIntegral $ hash (unpackFS knownHashLabel))
            ]
        ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveKnownHash ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CKnownHash
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveKnownHash rn orig (L _ lbl) = do
    ev <- evidenceKnownFieldLabel rn lbl
    return (Just (ev, orig), [])
