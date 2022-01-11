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

-- TODO: Will need extension for the polymorphic case
data Fields =
    FieldsCons Field Fields
  | FieldsNil
  | FieldsVar TyVar

data Field = Field FieldLabel Type

data FieldLabel =
    FieldKnown FastString
  | FieldVar   TyVar
  deriving (Eq)

-- | Find field type by name
findField :: FastString -> Fields -> Maybe Type
findField nm = go
  where
    go :: Fields -> Maybe Type
    go (FieldsCons (Field label typ) fs)
      | label == FieldKnown nm = Just typ
      | otherwise              = go fs
    go FieldsNil     = Nothing
    go (FieldsVar _) = Nothing

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
--
-- TODO: For our current 'Fields' definition, this will /always/ be the case,
-- but if we extend the parser to deal with field name variables or list
-- variables, this will no longer be the case.
allFieldsKnown :: Fields -> Maybe (KnownRecord ())
allFieldsKnown = go []
  where
    go :: [(FastString, KnownField ())]
       -> Fields
       -> Maybe (KnownRecord ())
    go acc = \case
        FieldsNil ->
          Just KnownRecord {
              knownFields = reverse acc
            }
        FieldsCons (Field label typ) fs ->
          case label of
            FieldKnown nm ->
              go ((nm, knownField typ) : acc) fs
            FieldVar _ ->
              Nothing
        FieldsVar _ ->
          Nothing

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
  ppr FieldsNil       = text "FieldsNil"
  ppr (FieldsVar var) = parens $ text "FieldsVar" <+> ppr var

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

parseFields :: Type -> Maybe Fields
parseFields fields = asum [
      do (f, fs) <- parseCons fields
         f' <- parseField f
         (FieldsCons f') <$> parseFields fs
    , do parseNil fields
         return FieldsNil
    , do FieldsVar <$> getTyVar_maybe fields
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
