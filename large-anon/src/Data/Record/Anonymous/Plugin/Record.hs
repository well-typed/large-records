{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
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
  , knownRecordFields
  , knownRecordFromFields
  , knownRecordIsomorphic
  , knownRecordTraverse
    -- ** Construction
  , checkAllFieldsKnown
    -- * Parsing
  , parseRecord
  , parseFields
  , parseFieldLabel
  ) where

import Control.Monad
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector         as V

import Data.Record.Anonymous.Internal.FieldName (FieldName)
import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.TyConSubst

import qualified Data.Record.Anonymous.Internal.FieldName    as FieldName
import qualified Data.Record.Anonymous.Internal.Util.HashMap as HashMap

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
    FieldKnown FieldName
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
-- Put another way: unlike in 'checkAllFieldsKnown', we do not insist that /all/
-- fields are known here, but only the fields up to (including) the one we're
-- looking for.
--
-- Returns the index and the type of the field, if found.
findField :: FieldName -> Fields -> Maybe (Int, Type)
findField nm = go 0 . (:[])
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

{-------------------------------------------------------------------------------
  Records of statically known shape
-------------------------------------------------------------------------------}

-- | Context-free information about a field in a record
--
-- In other words, we do /not/ know the /index/ of the field here, as that
-- depends the context (the particular record it is part of).
data KnownField a = KnownField {
      knownFieldName :: FieldName
    , knownFieldType :: Type
    , knownFieldInfo :: a
    }
  deriving (Functor, Foldable)

-- | Record with statically known shape
data KnownRecord a = KnownRecord {
      -- | Information about each field in the record, in user-specified order.
      --
      -- Order matters, because records with the same fields in a different
      -- order are not considered equal by the library (merely isomorphic).
      --
      -- May contain duplicates (if fields are shadowed).
      knownRecordVector :: Vector (KnownField a)

      -- | "Most recent" position of each field in the record
      --
      -- Shadowed fields are not included in this map.
      --
      -- Invariant:
      --
      -- >     Map.lookup n knownRecordNames == Just i
      -- > ==> knownFieldName (knownRecordFields V.! i) == n
    , knownRecordVisible :: HashMap FieldName Int
    }
  deriving (Functor, Foldable)

knownRecordFields :: KnownRecord a -> [KnownField a]
knownRecordFields = V.toList . knownRecordVector

knownRecordTraverse :: forall m a b.
     Applicative m
  => KnownRecord a
  -> (KnownField a -> m b)
  -> m (KnownRecord b)
knownRecordTraverse KnownRecord{..} f =
    mkRecord <$> traverse f' knownRecordVector
  where
    mkRecord :: Vector (KnownField b) -> KnownRecord b
    mkRecord updated = KnownRecord {
          knownRecordVector  = updated
        , knownRecordVisible = knownRecordVisible
        }

    f' :: KnownField a -> m (KnownField b)
    f' fld@(KnownField nm typ _info) = KnownField nm typ <$> f fld

knownRecordVisibleMap :: KnownRecord a -> HashMap FieldName (KnownField a)
knownRecordVisibleMap KnownRecord{..} =
    (knownRecordVector V.!) <$> knownRecordVisible

knownRecordFromFields :: forall a.
     [KnownField a]
     -- ^ Fields of the record in the order they appear in the row types
     --
     -- In other words, fields earlier in the list shadow later fields.
  -> KnownRecord a
knownRecordFromFields = go [] 0 HashMap.empty
  where
    go :: [KnownField a]        -- Acc fields, reverse order (includes shadowed)
       -> Int                   -- Next index
       -> HashMap FieldName Int -- Acc indices of visible fields
       -> [KnownField a]        -- To process
       -> KnownRecord a
    go accFields !nextIndex !accVisible = \case
        [] -> KnownRecord {
            knownRecordVector  = V.fromList $ reverse accFields
          , knownRecordVisible = accVisible
          }
        f:fs
          | name `HashMap.member` accVisible ->
              go (f : accFields)
                 (succ nextIndex)
                 accVisible        -- Field shadowed
                 fs
          | otherwise ->
              go (f : accFields)
                 (succ nextIndex)
                 (HashMap.insert name nextIndex accVisible)
                 fs
          where
            name = knownFieldName f

-- | Check if two known records are isomorphic
--
-- Returns the set of fields that are missing in either of the two records,
-- and the fields that match. If the first list is empty, the records are
-- isomorphic.
knownRecordIsomorphic :: forall a b.
     KnownRecord a
  -> KnownRecord b
  -> ([FieldName], [(FieldName, (KnownField a, KnownField b))])
knownRecordIsomorphic = \ra rb ->
      partitionEithers
    . map (uncurry aux)
    . HashMap.toList
    $ HashMap.merge (knownRecordVisibleMap ra) (knownRecordVisibleMap rb)
  where
    aux ::
         FieldName
      -> HashMap.Merged (KnownField a) (KnownField b)
      -> Either FieldName (FieldName, (KnownField a, KnownField b))
    aux k =
        HashMap.merged
          (Left . either knownFieldName knownFieldName)
          (Right . (k,))

-- | Return map from field name to type, /if/ all fields are statically known
checkAllFieldsKnown :: Fields -> Maybe (KnownRecord ())
checkAllFieldsKnown = go [] . (:[])
  where
    go :: [KnownField ()]
       -> [Fields]
       -> Maybe (KnownRecord ())
    go acc []       = Just $ knownRecordFromFields (reverse acc)
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

    knownField :: FieldName -> Type -> KnownField ()
    knownField nm typ = KnownField {
          knownFieldName = nm
        , knownFieldType = typ
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

instance Outputable a => Outputable (KnownRecord a) where
  ppr = ppr . knownRecordFields

instance Outputable a => Outputable (KnownField a) where
  ppr (KnownField name typ info) = parens $
          text "KnownField"
      <+> ppr name
      <+> ppr typ
      <+> ppr info

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

-- | Parse @Record f r@
--
-- Returns the argument @f, r@
parseRecord :: TyConSubst -> ResolvedNames -> Type -> Maybe (Type, Type)
parseRecord tcs ResolvedNames{..} t = do
    args <- parseInjTyConApp tcs tyConRecord t
    case args of
      [f, r]     -> Just (f, r)
      _otherwise -> Nothing

parseFields :: TyConSubst -> ResolvedNames -> Type -> Maybe Fields
parseFields tcs ResolvedNames{..} = go
  where
    go :: Type -> Maybe Fields
    go fields = asum [
          do (f, fs) <- parseCons tcs fields
             f' <- parseField tcs f
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

parseField :: TyConSubst -> Type -> Maybe Field
parseField tcs field = do
    (label, typ) <- parsePair tcs field
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
