{-# LANGUAGE RecordWildCards #-}

-- | Parsed form of a row type in the source
--
-- Intended for qualified import.
--
-- import Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow (Fields)
-- import qualified Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow as ParsedRow
module Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow (
    -- * Definition
    Fields     -- opaque
  , FieldLabel(..)
    -- * Check if all fields are known
  , allKnown
    -- * Parsing
  , parseFields
  , parseFieldLabel
  ) where

import Prelude hiding (lookup)

import Control.Monad (mzero)
import Control.Monad.State (State, evalState, state)
import Data.Foldable (asum)

import Data.Record.Anon.Internal.Core.FieldName (FieldName)

import qualified Data.Record.Anon.Internal.Core.FieldName as FieldName

import Data.Record.Anon.Internal.Plugin.TC.Row.KnownField (KnownField(..))
import Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow (KnownRow(..), KnownRowField(..))
import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution (ResolvedNames(..))
import Data.Record.Anon.Internal.Plugin.TC.Parsing

import qualified Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow as KnownRow

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Fields =
    FieldsCons Field Fields
  | FieldsNil
  | FieldsVar TyVar
  | FieldsMerge Fields Fields

data Field = Field FieldLabel Type

data FieldLabel =
    FieldKnown FieldName
  | FieldVar   TyVar
  deriving (Eq)

{-------------------------------------------------------------------------------
  Check if all fields are known
-------------------------------------------------------------------------------}

-- | Return map from field name to type, /if/ all fields are statically known
allKnown :: Fields -> Maybe (KnownRow Type)
allKnown =
    go [] . (:[])
  where
    go :: [KnownField Type]
       -> [Fields]
       -> Maybe (KnownRow Type)
    go acc []       = Just $ postprocess (reverse acc)
    go acc (fs:fss) =
        case fs of
          FieldsNil ->
            go acc fss
          FieldsCons (Field (FieldKnown nm) typ) fs' ->
            go (knownField nm typ : acc) (fs':fss)
          FieldsCons (Field (FieldVar _) _) _ ->
            Nothing
          FieldsVar _ ->
            Nothing
          FieldsMerge l r ->
            go acc (l:r:fss)

    knownField :: FieldName -> Type -> KnownField Type
    knownField nm typ = KnownField {
          knownFieldName = nm
        , knownFieldInfo = typ
        }

    -- Assign field indices
    postprocess :: [KnownField Type] -> KnownRow Type
    postprocess fields =
          KnownRow.fromList
        . flip evalState (length fields)
        . mapM assignIndex
        $ fields
      where
        assignIndex :: KnownField Type -> State Int (KnownRowField Type)
        assignIndex field = state $ \ix -> (
              KnownRow.toKnownRowField field (ix - 1)
            , pred ix
            )

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseFields :: TyConSubst -> ResolvedNames -> Type -> Maybe Fields
parseFields tcs rn@ResolvedNames{..} = go
  where
    go :: Type -> Maybe Fields
    go fields = asum [
          do (f, fs) <- parseCons tcs fields
             f' <- parseField tcs rn f
             (FieldsCons f') <$> go fs
        , do parseNil tcs fields
             return FieldsNil
        , do FieldsVar <$> getTyVar_maybe fields
        , do args <- parseInjTyConApp tcs tyConMerge fields
             (left, right) <- case args of
                                [_k, l, r]     -> return (l, r)
                                _otherwise -> mzero
             FieldsMerge <$> go left <*> go right
        ]

parseField :: TyConSubst -> ResolvedNames -> Type -> Maybe Field
parseField tcs rn field = do
    (label, typ) <- parsePair tcs rn field
    label' <- parseFieldLabel label
    return $ Field label' typ

parseFieldLabel :: Type -> Maybe FieldLabel
parseFieldLabel label = asum [
      fieldKnown <$> isStrLitTy     label
    , FieldVar   <$> getTyVar_maybe label
    ]
  where
    fieldKnown :: FastString -> FieldLabel
    fieldKnown = FieldKnown . FieldName.fromFastString

-- | Parse @(x := y)@
parsePair :: TyConSubst -> ResolvedNames -> Type -> Maybe (Type, Type)
parsePair tcs ResolvedNames{..} t = do
    args <- parseInjTyConApp tcs tyConPair t
    case args of
      [_kx, _ky, x, y] -> Just (x, y)
      _otherwise       -> Nothing

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable Fields where
  ppr (FieldsCons f fs) = parens $
          text "FieldsCons"
      <+> ppr f
      <+> ppr fs
  ppr FieldsNil         = text "FieldsNil"
  ppr (FieldsVar var)   = parens $ text "FieldsVar" <+> ppr var
  ppr (FieldsMerge l r) = parens $ text "Merge" <+> ppr l <+> ppr r

instance Outputable Field where
  ppr (Field label typ) = parens $
          text "Field"
      <+> ppr label
      <+> ppr typ

instance Outputable FieldLabel where
  ppr (FieldKnown nm)  = parens $ text "FieldKnown" <+> ppr nm
  ppr (FieldVar   var) = parens $ text "FieldVar"   <+> ppr var

