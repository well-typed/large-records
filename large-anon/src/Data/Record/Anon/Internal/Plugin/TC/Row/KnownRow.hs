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
    -- * Construction
  , fromList
  , toList
  , visibleMap
    -- * Combinators
  , traverse
  , indexed
    -- * Check for projections
  , CannotProject(..)
  , canProject
  ) where

import Prelude hiding (traverse)
import qualified Prelude

import Control.Monad.State (State, evalState, state)
import Data.Either (partitionEithers)
import Data.Vector (Vector)

import qualified Data.Vector  as V

import Data.Record.Anon.Internal.Core.FieldName (FieldName)
import Data.Record.Anon.Internal.Core.Util.SmallHashMap (SmallHashMap)

import Data.Record.Anon.Internal.Plugin.TC.Row.KnownField (KnownField(..))
import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI

import qualified Data.Record.Anon.Internal.Core.Util.SmallHashMap as HashMap

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
    , knownRecordVisible :: SmallHashMap FieldName Int

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

visibleMap :: KnownRow a -> SmallHashMap FieldName (KnownField a)
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
    go :: [KnownField a]  -- Acc fields, reverse order (includes shadowed)
       -> Int             -- Next index
       -> SmallHashMap FieldName Int -- Acc indices of visible fields
       -> Bool            -- Are all already processed fields visible?
       -> [KnownField a]  -- To process
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
    mkRow <$> Prelude.traverse f' knownRecordVector
  where
    mkRow :: Vector (KnownField b) -> KnownRow b
    mkRow updated = KnownRow {
          knownRecordVector     = updated
        , knownRecordVisible    = knownRecordVisible
        , knownRecordAllVisible = knownRecordAllVisible
        }

    f' :: KnownField a -> m (KnownField b)
    f' (KnownField nm info) = KnownField nm <$> f nm info

indexed :: KnownRow a -> KnownRow (Int, a)
indexed r =
    flip evalState 0 $
      traverse r (const aux)
  where
    aux :: a -> State Int (Int, a)
    aux a = state $ \i -> ((i, a), succ i)

{-------------------------------------------------------------------------------
  Check for projections
-------------------------------------------------------------------------------}

-- | Reason why we cannot project
data CannotProject =
    -- | We do not support projecting to records with shadowed fields
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
  deriving (Show, Eq)

-- | Check if we can project from one record to another
--
-- If the projection is possible, returns the paired information from both
-- records in the order of the /target/ record along with the index into the
-- /source/ record.
--
-- See docstring of the  'Project' class for some discussion of shadowing.
canProject :: forall a b.
     KnownRow a
  -> KnownRow b
  -> Either CannotProject [(Int, (a, b))]
canProject recordA recordB =
    if not (knownRecordAllVisible recordB) then
      Left TargetContainsShadowedFields
    else
        uncurry checkMissing
      . partitionEithers
      $ map findInA (toList recordB)
  where
    findInA :: KnownField b -> Either FieldName (Int, (a, b))
    findInA b =
        case HashMap.lookup (knownFieldName b) (visibleMap (indexed recordA)) of
          Nothing -> Left  $ knownFieldName b
          Just a  -> Right $ distrib (knownFieldInfo a, knownFieldInfo b)

    checkMissing :: [FieldName] -> x -> Either CannotProject x
    checkMissing []      x = Right x
    checkMissing missing _ = Left $ SourceMissesFields missing

    distrib :: ((i, a), b) -> (i, (a, b))
    distrib ((i, a), b) = (i, (a, b))

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable a => Outputable (KnownRow a) where
  ppr = ppr . toList
