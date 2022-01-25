{-# LANGUAGE BangPatterns        #-}
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
import Control.Monad.State
import Data.Foldable (asum)
import Data.List (sortOn)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Map            as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Vector         as V

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.TyConSubst

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
-- Put another way: unlike in 'checkAllFieldsKnown', we do not insist that /all/
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

-- | Context-free information about a field in a record
--
-- In other words, we do /not/ know the /index/ of the field here, as that
-- depends the context (the particular record it is part of).
data KnownField a = KnownField {
      knownFieldName :: FastString
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
      knownRecordVector :: Vector (KnownField a)

      -- | Position of each field in the record
      --
      -- Invariant:
      --
      -- >     Map.lookup n knownRecordNames == Just i
      -- > ==> knownFieldName (knownRecordFields V.! i) == n
    , knownRecordIndices :: Map FastString Int
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
        , knownRecordIndices = knownRecordIndices
        }

    f' :: KnownField a -> m (KnownField b)
    f' fld@(KnownField nm typ _info) = KnownField nm typ <$> f fld

knownRecordMap :: forall a. KnownRecord a -> Map FastString (Int, KnownField a)
knownRecordMap KnownRecord{..} = Map.map aux knownRecordIndices
  where
    aux :: Int -> (Int, KnownField a)
    aux ix = (ix, knownRecordVector V.! ix)

knownRecordFromFields :: forall a.
     [KnownField a]
     -- ^ Fields of the record in the order they appear in the row types
     --
     -- In other words, fields earlier in the list shadow later fields.
  -> KnownRecord a
knownRecordFromFields = go [] Map.empty 0
  where
    go :: [KnownField a]      -- Accumulated fields, reverse order
       -> Map FastString Int  -- Accumulated indices
       -> Int                 -- Next available index
       -> [KnownField a]      -- To process
       -> KnownRecord a
    go accFields !accIndices !nextIndex = \case
        [] -> KnownRecord {
            knownRecordVector  = V.fromList $ reverse accFields
          , knownRecordIndices = accIndices
          }
        f:fs
          | name `Map.member` accIndices ->
              -- Field shadowed
              go accFields accIndices nextIndex fs
          | otherwise ->
              go (f:accFields)
                 (Map.insert name nextIndex accIndices)
                 (succ nextIndex)
                 fs
          where
            name = knownFieldName f

-- | Check if two known records are isomorphic
--
-- We do not check type equalities here, and instead just return which pairs
-- of types should be equal. These should be returned as new wanted constraints.
knownRecordIsomorphic :: forall a b.
     KnownRecord a
  -> KnownRecord b
  -> ( Map FastString ((Type, Type), (a, b))  -- Matched fields
     , [KnownField a]  -- Fields only in the left  (in original order)
     , [KnownField b]  -- Fields only in the right (in original order)
     )
knownRecordIsomorphic = \ra rb ->
    let (inBoth, (onlyInLeft, onlyInRight)) = flip runState ([], []) $
            Map.mergeA
              (Map.traverseMaybeMissing inLeft)
              (Map.traverseMaybeMissing inRight)
              (Map.zipWithAMatched presentInBoth)
              (knownRecordMap ra)
              (knownRecordMap rb)
     in ( inBoth
        , map snd $ sortOn fst onlyInLeft
        , map snd $ sortOn fst onlyInRight
        )
  where
    -- We ignore the indices here; we could potentially use them to order the
    -- matching fields according to one of the two records
    presentInBoth ::
         FastString
      -> (Int, KnownField a)
      -> (Int, KnownField b)
      -> State ([(Int, KnownField a)], [(Int, KnownField b)])
               ((Type, Type), (a, b))
    presentInBoth _nm (_ia, fa) (_ib, fb) = return (
          (knownFieldType fa, knownFieldType fb)
        , (knownFieldInfo fa, knownFieldInfo fb)
        )

    inLeft ::
         FastString
      -> (Int, KnownField a)
      -> State ([(Int, KnownField a)], [(Int, KnownField b)])
               (Maybe x)
    inLeft _nm (ia, fa) = state $ \(l, r) -> (Nothing, ((ia, fa) : l, r))

    inRight ::
         FastString
      -> (Int, KnownField b)
      -> State ([(Int, KnownField a)], [(Int, KnownField b)])
               (Maybe x)
    inRight _nm (ib, fb) = state $ \(l, r) -> (Nothing, (l, (ib, fb) : r))

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

    knownField :: FastString -> Type -> KnownField ()
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

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

-- | Parse @Record r@
--
-- Returns the argument @r@
parseRecord :: TyConSubst -> ResolvedNames -> Type -> Maybe Type
parseRecord tcs ResolvedNames{..} t = do
    args <- parseInjTyConApp tcs tyConRecord t
    case args of
      [r]        -> Just r
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
      FieldKnown <$> isStrLitTy     label
    , FieldVar   <$> getTyVar_maybe label
    ]
