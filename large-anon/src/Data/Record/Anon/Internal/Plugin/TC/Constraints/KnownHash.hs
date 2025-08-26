{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anon.Internal.Plugin.TC.Constraints.KnownHash (
    CKnownHash(..)
  , parseKnownHash
  , solveKnownHash
  ) where

import Data.Hashable (hash)
import Data.Void

import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Parsing

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
    parseConstraint isRelevant (text "isRelevant") $ \(ty, label) -> do
      return $ CKnownHash {
          knownHashLabel = label
        , knownHashType  = ty
        }
  where
    isRelevant :: Class -> [Type] -> Maybe (Type, FastString)
    isRelevant cls args
      | [ty] <- args
      , cls == clsKnownHash
      , Just label <- isStrLitTy ty
      = Just (ty, label)

      | otherwise
      = Nothing

evidenceKnownFieldLabel ::
     ResolvedNames
  -> CKnownHash
  -> TcPluginM 'Solve EvTerm
evidenceKnownFieldLabel ResolvedNames{..} CKnownHash{..} =
    return $ EvExpr $
      evDataConApp
        (classDataCon clsKnownHash)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceKnownHash) $ concat [
              map Type typeArgsEvidence
            , [ mkUncheckedIntExpr . fromIntegral $
                  hash (unpackFS knownHashLabel)
              ]
            ]
        ]
  where
    typeArgsEvidence :: [Type]
    typeArgsEvidence = [
          knownHashType
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
