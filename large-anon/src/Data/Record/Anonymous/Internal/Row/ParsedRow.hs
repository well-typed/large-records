{-# LANGUAGE RecordWildCards #-}

-- | Parsed form of a row type in the source
--
-- Intended for qualified import.
--
-- import Data.Record.Anonymous.Internal.Row.ParsedRow (Fields)
-- import qualified Data.Record.Anonymous.Internal.Row.ParsedRow as ParsedRow
module Data.Record.Anonymous.Internal.Row.ParsedRow (
    -- * Definition
    Fields     -- opaque
  , FieldLabel(..)
    -- * Query
  , lookup
  , allKnown
    -- * Parsing
  , parseFields
  , parseFieldLabel
  ) where

import Prelude hiding (lookup)

import Control.Monad (mzero)
import Data.Foldable (asum)

import Data.Record.Anon.Internal.Core.FieldName (FieldName)

import qualified Data.Record.Anon.Internal.Core.FieldName as FieldName

import Data.Record.Anonymous.Internal.Row.KnownField (KnownField(..))
import Data.Record.Anonymous.Internal.Row.KnownRow (KnownRow(..))
import Data.Record.Anonymous.TcPlugin.GhcTcPluginAPI
import Data.Record.Anonymous.TcPlugin.NameResolution (ResolvedNames(..))
import Data.Record.Anonymous.TcPlugin.Parsing
import Data.Record.Anonymous.TcPlugin.TyConSubst (TyConSubst)

import qualified Data.Record.Anonymous.Internal.Row.KnownRow as KnownRow

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
  Query
-------------------------------------------------------------------------------}

-- | Find field type by name
--
-- Since records are left-biased, we report the /first/ match, independent of
-- what is in the record tail. If however we encounter an unknown (variable)
-- field, we stop the search: even if a later field matches the one we're
-- looking for, the unknown field might too and, crucially, might not have the
-- same type.
--
-- Put another way: unlike in 'checkAllFieldsKnown', we do not insist that /all/
-- fields are known here, but only the fields up to (including) the one we're
-- looking for.
--
-- Returns the index and the type of the field, if found.
lookup :: FieldName -> Fields -> Maybe (Int, Type)
lookup nm = go 0 . (:[])
  where
    go :: Int -> [Fields] -> Maybe (Int, Type)
    go _ []       = Nothing
    go i (fs:fss) =
        case fs of
          FieldsNil ->
            go i fss
          FieldsVar _ ->
            -- The moment we encounter a variable (unknown part of the record),
            -- we must say that the field is unknown (see discussion above)
            Nothing
          FieldsCons (Field (FieldKnown nm') typ) fs' ->
            if nm == nm' then
              Just (i, typ)
            else
              go (succ i) (fs':fss)
          FieldsCons (Field (FieldVar _) _) _ ->
            -- We must also stop when we see a field with an unknown name
            Nothing
          FieldsMerge l r ->
            go i (l:r:fss)


-- | Return map from field name to type, /if/ all fields are statically known
allKnown :: Fields -> Maybe (KnownRow Type)
allKnown
 = go [] . (:[])
  where
    go :: [KnownField Type]
       -> [Fields]
       -> Maybe (KnownRow Type)
    go acc []       = Just $ KnownRow.fromList (reverse acc)
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
                                [l, r]     -> return (l, r)
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

