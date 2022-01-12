{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Description of a record as it is known at compile time
module Data.Record.Anonymous.Plugin.Record (
    -- * General case
    Fields(..)
  , Field(..)
  , FieldLabel(..)
    -- ** Query
  , findField
    -- * Records of statically known shape
  , KnownRecord(..)
  , KnownField(..)
    -- ** Construction
  , allFieldsKnown
  , forKnownRecord
    -- * Parsing
  , parseRecord
  , parseFields
  , parseFieldLabel
  ) where

import Control.Monad
import Data.Foldable (asum)
import Data.Traversable (for)

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing

{-------------------------------------------------------------------------------
  General case
-------------------------------------------------------------------------------}

data Fields =
    FieldsCons Field Fields
  | FieldsNil
  | FieldsVar TyVar
  | FieldsMerge Fields Fields

data Field = Field FieldLabel Type

data FieldLabel =
    FieldKnown FastString
  | FieldVar   TyVar
  deriving (Eq)

-- | Find field type by name
--
-- Since records are left-biased, we report the /first/ match, independent of
-- what is in the record tail. If however we encounter an unknown (variable)
-- field, we stop the search: even if a later field matches the one we're
-- looking for, the unknown field might too and, crucially, might not have the
-- same type.
--
-- Put another way: unlike in 'allFieldsKnown', we do not insist that /all/
-- fields are known here, but only the fields up to (including) the one we're
-- looking for.
findField :: FastString -> Fields -> Maybe Type
findField nm = go . (:[])
  where
    go :: [Fields] -> Maybe Type
    go []       = Nothing
    go (fs:fss) =
        case fs of
          FieldsNil ->
            go fss
          FieldsVar _ ->
            -- The moment we encounter a variable (unknown part of the record),
            -- we must say that the field is unknown (see discussion above)
            Nothing
          FieldsCons (Field (FieldKnown nm') typ) fs' ->
            if nm == nm' then
              Just typ
            else
              go (fs':fss)
          FieldsCons (Field (FieldVar _) _) _ ->
            -- We must also stop when we see a field with an unknown name
            Nothing
          FieldsMerge l r ->
            go (l:r:fss)

{-------------------------------------------------------------------------------
  Records of statically known shape
-------------------------------------------------------------------------------}

data KnownRecord a = KnownRecord {
      -- | Known fields, in order
      --
      -- The order of the known fields matches the order as specified in the
      -- (user-defined) type. This is important, because the @large-anon@
      -- library considers records with re-ordered fields to not be equal
      -- (merely isomorphic, see 'castRecord').
      knownFields :: [(FastString, KnownField a)]
    }
  deriving (Functor, Foldable)

data KnownField a = KnownField {
      knownFieldType :: Type
    , knownFieldInfo :: a
    }
  deriving (Functor, Foldable)

forKnownRecord :: forall m a b.
     Applicative m
  => KnownRecord a
  -> (FastString -> Type -> a -> m b)
  -> m (KnownRecord b)
forKnownRecord (KnownRecord fields) f = fmap KnownRecord $
    for fields $ \(nm, fld) ->
      (nm,) <$> aux nm fld
  where
    aux :: FastString -> KnownField a -> m (KnownField b)
    aux nm (KnownField typ a) = KnownField typ <$> f nm typ a

-- | Return map from field name to type, /if/ all fields are statically known
allFieldsKnown :: Fields -> Maybe (KnownRecord ())
allFieldsKnown = go [] . (:[])
  where
    go :: [(FastString, KnownField ())]
       -> [Fields]
       -> Maybe (KnownRecord ())
    go acc []       = Just KnownRecord { knownFields = reverse acc }
    go acc (fs:fss) =
        case fs of
          FieldsNil ->
            go acc fss
          FieldsCons (Field (FieldKnown nm) typ) fs' ->
            go ((nm, knownField typ) : acc) (fs':fss)
          FieldsCons (Field (FieldVar _) _) _ ->
            Nothing
          FieldsVar _ ->
            Nothing
          FieldsMerge l r ->
            go acc (l:r:fss)

    knownField :: Type -> KnownField ()
    knownField typ = KnownField {
          knownFieldType = typ
        , knownFieldInfo = ()
        }

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

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

-- | Parse @Record r@
--
-- Returns the argument @r@
parseRecord :: ResolvedNames -> Type -> Maybe Type
parseRecord ResolvedNames{..} r = asum [
      do (tyRecord, tyFields) <- splitAppTy_maybe r
         tcRecord <- tyConAppTyCon_maybe tyRecord
         guard $ tcRecord == tyConRecord
         return tyFields
    ]

parseFields :: ResolvedNames -> Type -> Maybe Fields
parseFields ResolvedNames{..} = go
  where
    go :: Type -> Maybe Fields
    go fields = asum [
          do (f, fs) <- parseCons fields
             f' <- parseField f
             (FieldsCons f') <$> go fs
        , do parseNil fields
             return FieldsNil
        , do FieldsVar <$> getTyVar_maybe fields
        , do (tyCon, args) <- splitTyConApp_maybe fields
             guard $ tyCon == tyConMerge
             (left, right) <- case args of
                                [l, r]     -> return (l, r)
                                _otherwise -> mzero
             FieldsMerge <$> go left <*> go right
        ]

parseField :: Type -> Maybe Field
parseField field = do
    (label, typ) <- parsePair field
    label' <- parseFieldLabel label
    return $ Field label' typ

parseFieldLabel :: Type -> Maybe FieldLabel
parseFieldLabel label = asum [
      FieldKnown <$> isStrLitTy     label
    , FieldVar   <$> getTyVar_maybe label
    ]
