{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Used by the plugin for evidence construction during constraint resolution
--
-- This module is exclusively used by the plugin; it is not used anywhere
-- inside the rest of the library: there are no imports of this module.
module Data.Record.Anonymous.Internal.Evidence (
    evidenceHasField
  , evidenceAllFields
  , evidenceKnownFields
  ) where

import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)
import Data.SOP.Dict
import GHC.Exts (Any)

import qualified Data.Vector as Vector

import Data.Record.Anonymous.Internal.Record
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Internal.Diff as Diff
import Data.Record.Generic (FieldMetadata)

-- | Evidence for 'HasField'
--
-- Precondition: the record must have the specified field with type @a@, (where
-- @a@ will be of the form @f a'@ for some @a'). This precondition is verified
-- by the plugin before generating "evidence" that uses this function.
--
-- TODO: Since we only resolve HasField if we know the (prefix) of a row, we
-- could easily produce a vector offset here (along with the name, for 'Diff')?
-- If we do, do we still need the names at all in Canonical? If not, we might
-- be able to drop the 'RowMetadata' constraint from the 'Rep' conversions and
-- therefore also as a superclass constraint on the 'Generic' instance.
evidenceHasField :: forall f r a.
     String
  -> Record f r
  -> (a -> Record f r, a)
evidenceHasField label r@Record{..} = (
      \x -> r { recordDiff = Diff.set label (co' x) recordCanon recordDiff }
    , co $ Diff.get label recordDiff recordCanon
    )
  where
    co  :: f Any -> a
    co' :: a -> f Any

    co =  noInlineUnsafeCo
    co' = noInlineUnsafeCo

-- | Evidence for 'ConstrainRow'
evidenceAllFields :: [Dict c Any] -> AllFieldsDict r c
evidenceAllFields dicts _ _ = Vector.fromList dicts

-- | Evidence for 'KnownFields'
evidenceKnownFields :: [FieldMetadata Any] -> KnownFieldsDict r
evidenceKnownFields metadata _ = metadata
