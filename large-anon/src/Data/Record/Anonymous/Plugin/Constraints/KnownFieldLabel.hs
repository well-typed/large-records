{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.KnownFieldLabel (
    CKnownFieldLabel(..)
  , parseKnownFieldLabel
  , evidenceKnownFieldLabel
  , solveKnownFieldLabel
  ) where

import Data.Void

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.TyConSubst

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of an @KnownFieldLabel f@ constraint
data CKnownFieldLabel = CKnownFieldLabel {
      -- | The raw type argument to the @KnownFieldLabel@ constraint.
      knownFieldType :: Type
      -- | The underlying @FastString@ when the label is a literal.
    , knownFieldLabel :: FastString
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CKnownFieldLabel where
  ppr (CKnownFieldLabel _ field) = parens $
          text "CKnownFieldLabel"
      <+> ppr field

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseKnownFieldLabel ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CKnownFieldLabel)
parseKnownFieldLabel _ ResolvedNames{..} =
    parseConstraint' clsKnownFieldLabel $ \[ty] -> do
      fld <- isStrLitTy ty
      return $ CKnownFieldLabel {
            knownFieldType  = ty
          , knownFieldLabel = fld
          }

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceKnownFieldLabel :: ResolvedNames -> CKnownFieldLabel -> EvExpr
evidenceKnownFieldLabel ResolvedNames{..} CKnownFieldLabel{..} =
    mkCoreConApps
      (classDataCon clsKnownFieldLabel)
      [ Type knownFieldType
      , mkFastStringUniqueIntExpr knownFieldLabel ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveKnownFieldLabel ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CKnownFieldLabel
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveKnownFieldLabel rn orig (L _ lbl) = do
  let ev = evidenceKnownFieldLabel rn lbl
  return (Just (EvExpr ev, orig), [])
