{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.HasField (
    CHasField(..)
  , parseHasField
  , solveHasField
  ) where

import Control.Monad
import Data.Void
import GHC.Stack

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.Record
import Data.Record.Anonymous.Plugin.TyConSubst

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of a @HasField x r a@, where @r = Record f r'@
data CHasField = CHasField {
      -- | Label we're looking for (@x@)
      hasFieldLabel :: FieldLabel

      -- | Fields of the record (parsed form of @r'@)
      --
      -- These may be fully or partially known, or completely unknown.
    , hasFieldRecord :: Fields

      -- | Raw arguments to @HasField@ (for evidence construction)
    , hasFieldTypeRaw :: [Type]

      -- | Record functor argument (@f@)
    , hasFieldTypeFunctor :: Type

      -- | Type of the record (@r@)
    , hasFieldTypeRecord :: Type

      -- | Type of the record field we're looking for (@a@)
      --
      -- Although @a@ will be of the form @f a'@ for some @a'@, we do not
      -- enforce that here (but instead generate a new wanted equality
      -- constraint to enforce this).
    , hasFieldTypeField :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CHasField where
  ppr (CHasField label record typeRaw typeRecord typeFunctor typeField) = parens $
      text "CHasField" <+> braces (vcat [
          text "hasFieldLabel"       <+> text "=" <+> ppr label
        , text "hasFieldRecord"      <+> text "=" <+> ppr record
        , text "hasFieldTypeRaw"     <+> text "=" <+> ppr typeRaw
        , text "hasFieldTypeFunctor" <+> text "=" <+> ppr typeFunctor
        , text "hasFieldTypeRecord"  <+> text "=" <+> ppr typeRecord
        , text "hasFieldTypeField"   <+> text "=" <+> ppr typeField
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseHasField ::
     HasCallStack
  => TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CHasField)
parseHasField tcs rn@ResolvedNames{..} =
    parseConstraint isRelevant $ \(args, x, (f, tyFields), a) -> do
      label  <- parseFieldLabel x
      fields <- parseFields tcs rn tyFields

      return $ CHasField {
          hasFieldLabel       = label
        , hasFieldRecord      = fields
        , hasFieldTypeRaw     = args
        , hasFieldTypeFunctor = f
        , hasFieldTypeRecord  = tyFields
        , hasFieldTypeField   = a
        }
  where
    -- The constraint is relevant if
    --
    -- o It is of the form @HasField k x r a@
    -- o @k == Symbol@
    -- o @r == Record f r'@
    --
    -- If relevant, returns the raw arguments @[k, x, r, a]@ and @(x, (f, r'), a)@
    isRelevant :: Class -> [Type] -> Maybe ([Type], Type, (Type, Type), Type)
    isRelevant cls args@[k, x, r, a] = do
        guard $ cls == clsHasField
        tcSymbol <- tyConAppTyCon_maybe k -- TODO: equal up to equalities..?
        guard $ tcSymbol == typeSymbolKindCon
        tyFields <- parseRecord tcs rn r
        return (args, x, tyFields, a)
    isRelevant _ _ = Nothing

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceHasField ::
     ResolvedNames
  -> CHasField
  -> FastString -- ^ Field name (we cannot produce evidence for unknown fields)
  -> TcPluginM 'Solve EvTerm
evidenceHasField ResolvedNames{..} CHasField{..} name = do
    str <- mkStringExprFS name
    return $
      evDataConApp
        (classDataCon clsHasField)
        hasFieldTypeRaw
        [ mkCoreApps (Var idUnsafeRecordHasField) [
              Type hasFieldTypeFunctor
            , Type hasFieldTypeRecord
            , Type hasFieldTypeField
            , str
            ]
        ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveHasField ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CHasField
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveHasField _ _ (L _ CHasField{hasFieldLabel = FieldVar _}) =
    return (Nothing, [])
solveHasField rn orig (L loc hf@CHasField{hasFieldLabel = FieldKnown name, ..}) =
    case findField name hasFieldRecord of
      Nothing ->
        -- TODO: If the record is fully known, we should issue a custom type
        -- error here rather than leaving the constraint unsolved
        return (Nothing, [])
      Just typ -> do
        eq <- newWanted loc $
                mkPrimEqPredRole Nominal
                  hasFieldTypeField
                  (hasFieldTypeFunctor `mkAppTy` typ)
        ev <- evidenceHasField rn hf name
        return (Just (ev, orig), [mkNonCanonical eq])
