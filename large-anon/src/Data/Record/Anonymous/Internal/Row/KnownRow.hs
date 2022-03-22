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
-- > import Data.Record.Anonymous.Internal.Row.KnownRow (KnownRow(..))
-- > import qualified Data.Record.Anonymous.Internal.Row.KnownRow as KnownRow
module Data.Record.Anonymous.Internal.Row.KnownRow (
    -- * Definition
    KnownRow(..)
    -- * Construction
  , fromList
  , toList
  , visibleMap
    -- * Combinators
  , traverse
    -- * Check for projections
  , CannotProject(..)
  , canProject
  ) where

import Prelude hiding (traverse)
import qualified Prelude

import Data.Either (partitionEithers)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)

import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HashMap

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI

import Data.Record.Anonymous.Internal.Row.FieldName (FieldName)
import Data.Record.Anonymous.Internal.Row.KnownField (KnownField(..))
import Data.Record.Anonymous.Internal.Util.HashMap (Merged(..))

import qualified Data.Record.Anonymous.Internal.Util.HashMap as Util.HashMap

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
      knownRecordVector :: Vector (KnownField a)

      -- | "Most recent" position of each field in the record
      --
      -- Shadowed fields are not included in this map.
      --
      -- Invariant:
      --
      -- >     HashMap.lookup n knownRecordNames == Just i
      -- > ==> knownFieldName (knownRecordVector V.! i) == n
    , knownRecordVisible :: HashMap FieldName Int

      -- | Are all fields in this record visible?
      --
      -- 'False' if some fields are shadowed.
    , knownRecordAllVisible :: Bool
    }
  deriving (Functor, Foldable)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

toList :: KnownRow a -> [KnownField a]
toList = V.toList . knownRecordVector

visibleMap :: KnownRow a -> HashMap FieldName (KnownField a)
visibleMap KnownRow{..} = (knownRecordVector V.!) <$> knownRecordVisible

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromList :: forall a.
     [KnownField a]
     -- ^ Fields of the record in the order they appear in the row types
     --
     -- In other words, fields earlier in the list shadow later fields.
  -> KnownRow a
fromList = go [] 0 HashMap.empty True
  where
    go :: [KnownField a]        -- Acc fields, reverse order (includes shadowed)
       -> Int                   -- Next index
       -> HashMap FieldName Int -- Acc indices of visible fields
       -> Bool                  -- Are all already processed fields visible?
       -> [KnownField a]        -- To process
       -> KnownRow a
    go accFields !nextIndex !accVisible !accAllVisible = \case
        [] -> KnownRow {
            knownRecordVector     = V.fromList $ reverse accFields
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
            name = knownFieldName f

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

traverse :: forall m a b.
     Applicative m
  => KnownRow a
  -> (FieldName -> a -> m b)
  -> m (KnownRow b)
traverse KnownRow{..} f =
    mkRecord <$> Prelude.traverse f' knownRecordVector
  where
    mkRecord :: Vector (KnownField b) -> KnownRow b
    mkRecord updated = KnownRow {
          knownRecordVector     = updated
        , knownRecordVisible    = knownRecordVisible
        , knownRecordAllVisible = knownRecordAllVisible
        }

    f' :: KnownField a -> m (KnownField b)
    f' (KnownField nm info) = KnownField nm <$> f nm info

{-------------------------------------------------------------------------------
  Check for projections
-------------------------------------------------------------------------------}

-- | Reason why we cannot project
data CannotProject =
    -- | We do not supporting to records with shadowed fields
    --
    -- Since these fields can only come from the source record, and shadowed
    -- fields in the source record are invisible, shadowed fields in the target
    -- could only be duplicates of the same field in the source. This is not
    -- particularly useful, so we don't support it. Moreover, since we actually
    -- create /lenses/ from these projections, it is important that every field
    -- in the source record corresponds to at most /one/ field in the target.
    TargetContainsShadowedFields

    -- | Some fields in the target are missing in the source
  | SourceMissesFields [FieldName]

-- | Check if we can project from one record to another
--
-- See docstring of the  'Project' class for some discussion of shadowing.
canProject :: forall a b.
     KnownRow a
  -> KnownRow b
  -> Either CannotProject [(KnownField a, KnownField b)]
canProject recordA recordB =
    if not (knownRecordAllVisible recordB) then
      Left TargetContainsShadowedFields
    else
        uncurry checkMissing
      . partitionEithers
      . mapMaybe (uncurry checkField)
      . HashMap.toList
      $ Util.HashMap.merge visibleA visibleB
  where
    visibleA :: HashMap FieldName (KnownField a)
    visibleA = visibleMap recordA

    visibleB :: HashMap FieldName (KnownField b)
    visibleB = visibleMap recordB

    checkField ::
         FieldName
      -> Merged (KnownField a) (KnownField b)
      -> Maybe (Either FieldName (KnownField a, KnownField b))
    checkField n = \case
        -- A field that only appears on the LHS is irrelevant
        InLeft _ -> Nothing
        -- A field that only appears on the RHS is problematic
        InRight _ -> Just (Left n)
        -- A field that appears in both is part of the projection
        InBoth x y -> Just (Right (x, y))

    checkMissing ::
         [FieldName]
      -> [(KnownField a, KnownField b)]
      -> Either CannotProject [(KnownField a, KnownField b)]
    checkMissing []      matched = Right matched
    checkMissing missing _       = Left $ SourceMissesFields missing


{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable a => Outputable (KnownRow a) where
  ppr = ppr . toList
