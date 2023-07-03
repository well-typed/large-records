{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Information about a record
--
-- Intended for qualified import.
--
-- > import Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow (KnownRow(..))
-- > import qualified Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow as KnownRow
module Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow (
    -- * Definition
    KnownRow(..)
    -- * Fields
  , KnownRowField(..)
  , FieldIndex
  , toKnownRowField
  , fromKnownRowField
    -- * Conversion
  , fromList
  , inRowOrder
  , inFieldOrder
  , visibleMap
    -- * Query
  , lookup
    -- * Combinators
  , traverse
    -- * Check for subrows
  , NotSubRow(..)
  , Source(..)
  , Target(..)
  , isSubRowOf
  ) where

import Prelude hiding (traverse, lookup)
import qualified Prelude

import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Ord (comparing)

import Data.Record.Anon.Internal.Core.FieldName (FieldName)
import Data.Record.Anon.Internal.Util.SmallHashMap (SmallHashMap)

import Data.Record.Anon.Internal.Plugin.TC.Row.KnownField (KnownField(..))
import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI

import qualified Data.Record.Anon.Internal.Util.SmallHashMap as HashMap

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Record with statically known shape
data KnownRow a = KnownRow {
      -- | Information about each field in the record, in user-specified order.
      --
      -- Order matters, because records with the same fields in a different
      -- order are not considered equal by the library (merely isomorphic).
      --
      -- May contain duplicates (if fields are shadowed).
      knownRecordVector :: [KnownRowField a]

      -- | "Most recent" position of each field in the record
      --
      -- Shadowed fields are not included in this map.
      --
      -- Invariant:
      --
      -- >     HashMap.lookup n knownRecordNames == Just i
      -- > ==> knownFieldName (knownRecordVector V.! i) == n
    , knownRecordVisible :: SmallHashMap FieldName Int

      -- | Are all fields in this record visible?
      --
      -- 'False' if some fields are shadowed.
    , knownRecordAllVisible :: Bool
    }
  deriving (Functor)

{-------------------------------------------------------------------------------
  Individual fields
-------------------------------------------------------------------------------}

-- | Field in a known row
data KnownRowField a = KnownRowField {
      knownRowFieldName  :: FieldName
    , knownRowFieldIndex :: FieldIndex
    , knownRowFieldInfo  :: a
    }
  deriving (Functor)

type FieldIndex = Int

-- | Drop index information
fromKnownRowField :: KnownRowField a -> KnownField a
fromKnownRowField field = KnownField {
      knownFieldName = knownRowFieldName field
    , knownFieldInfo = knownRowFieldInfo field
    }

-- | Add index information
toKnownRowField :: KnownField a -> FieldIndex -> KnownRowField a
toKnownRowField field ix = KnownRowField {
      knownRowFieldName  = knownFieldName field
    , knownRowFieldInfo  = knownFieldInfo field
    , knownRowFieldIndex = ix
    }

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | List of all fields, in row order
--
-- This may /NOT/ be the order in which the fields are stored.
inRowOrder :: KnownRow a -> [KnownField a]
inRowOrder =
      map fromKnownRowField
    . knownRecordVector

-- | List of all fields, ordered by fieldIndex
inFieldOrder :: KnownRow a -> [KnownField a]
inFieldOrder =
      map fromKnownRowField
    . sortBy (comparing knownRowFieldIndex)
    . knownRecordVector

visibleMap :: KnownRow a -> SmallHashMap FieldName (KnownRowField a)
visibleMap KnownRow{..} = (knownRecordVector !!) <$> knownRecordVisible

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromList :: forall a.
     [KnownRowField a]
     -- ^ Fields of the record in the order they appear in the row types
     --
     -- In other words, fields earlier in the list shadow later fields.
  -> KnownRow a
fromList = go [] 0 HashMap.empty True
  where
    go :: [KnownRowField a]           -- Acc fields, rev order (incl shadowed)
       -> Int                         -- Next index
       -> SmallHashMap FieldName Int  -- Acc indices of visible fields
       -> Bool                        -- All already processed fields visible?
       -> [KnownRowField a]           -- To process
       -> KnownRow a
    go accFields !nextIndex !accVisible !accAllVisible = \case
        [] -> KnownRow {
            knownRecordVector     = reverse accFields
          , knownRecordVisible    = accVisible
          , knownRecordAllVisible = accAllVisible
          }
        f:fs
          | name `HashMap.member` accVisible ->
              -- Field shadowed
              go (f : accFields)
                 (succ nextIndex)
                 accVisible
                 False
                 fs
          | otherwise ->
              go (f : accFields)
                 (succ nextIndex)
                 (HashMap.insert name nextIndex accVisible)
                 accAllVisible
                 fs
          where
            name = knownRowFieldName f

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

lookup :: FieldName -> KnownRow a -> Maybe (KnownRowField a)
lookup field KnownRow{..} =
    (knownRecordVector !!) <$>
      HashMap.lookup field knownRecordVisible

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

traverse :: forall m a b.
     Applicative m
  => KnownRow a
  -> (FieldName -> FieldIndex -> a -> m b)
  -> m (KnownRow b)
traverse KnownRow{..} f =
    mkRow <$> Prelude.traverse f' knownRecordVector
  where
    mkRow :: [KnownRowField b] -> KnownRow b
    mkRow updated = KnownRow {
          knownRecordVector     = updated
        , knownRecordVisible    = knownRecordVisible
        , knownRecordAllVisible = knownRecordAllVisible
        }

    f' :: KnownRowField a -> m (KnownRowField b)
    f' (KnownRowField nm ix info) = KnownRowField nm ix <$> f nm ix info

{-------------------------------------------------------------------------------
  Check for projections
-------------------------------------------------------------------------------}

-- | Reason why we cannot failed to prove 'SubRow'
data NotSubRow =
    -- | We do not support records with shadowed fields
    --
    -- Since these fields can only come from the source record, and shadowed
    -- fields in the source record are invisible, shadowed fields in the target
    -- could only be duplicates of the same field in the source. This is not
    -- particularly useful, so we don't support it. Moreover, since we actually
    -- create /lenses/ from these subrows, it is important that every field in
    -- the source record corresponds to at most /one/ field in the target.
    TargetContainsShadowedFields

    -- | Some fields in the target are missing in the source
  | SourceMissesFields [FieldName]
  deriving (Show, Eq)

newtype Source a = Source { getSource :: a } deriving (Show, Functor)
newtype Target a = Target { getTarget :: a } deriving (Show, Functor)

-- | Check if one row is a subrow of another
--
-- If it is, returns the paired information from both records. If @a@ is a
-- subrow of @b@, then we can project from @b@ to @a@; for improved clarity,
-- we therefore mark @a@ as the /target/ and @b@ as the source.
--
-- Results are returned in row order of the target.
--
-- See 'NotSubRow' for some discussion of shadowing.
isSubRowOf :: forall a b.
     KnownRow a  -- ^ Target
  -> KnownRow b  -- ^ Source
  -> Either NotSubRow [(Target (KnownField a), Source (KnownRowField b))]
target `isSubRowOf` source =
    if not (knownRecordAllVisible target) then
      Left TargetContainsShadowedFields
    else
        uncurry checkMissing
      . partitionEithers
        -- It doesn't matter which order we process 'target' in:
      $ map findInSrc (inRowOrder target)
  where
    findInSrc ::
         KnownField a
      -> Either FieldName (Target (KnownField a), Source (KnownRowField b))
    findInSrc a =
        case HashMap.lookup (knownFieldName a) (visibleMap source) of
          Nothing -> Left  $ knownFieldName a
          Just b  -> Right $ (Target a, Source b)

    checkMissing :: [FieldName] -> x -> Either NotSubRow x
    checkMissing []      x = Right x
    checkMissing missing _ = Left $ SourceMissesFields missing

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable a => Outputable (KnownRow a) where
  ppr = ppr . inRowOrder
