{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Used by the plugin for evidence construction during constraint resolution
--
-- This module is exclusively used by the plugin; it is not used anywhere
-- inside the rest of the library: there are no imports of this module.
module Data.Record.Anonymous.Internal.Evidence (
    evidenceAllFields
  , evidenceHasField
  , evidenceIsomorphic
  , evidenceKnownFields
  , evidenceKnownHash
  ) where

import Data.Record.Generic (FieldMetadata)
import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)
import Data.SOP.Dict
import GHC.Exts (Any)

import qualified Data.Vector as Vector

import Data.Record.Anonymous.Internal.FieldName (FieldName(..), DictKnownHash)
import Data.Record.Anonymous.Internal.Record
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Internal.Diff as Diff

{-------------------------------------------------------------------------------
  'HasField'
-------------------------------------------------------------------------------}

-- | Evidence for 'HasField'
--
-- Precondition: the record must have the specified field with type @a@ (where
-- @a@ will be of the form @f a'@ for some @a'). This precondition is verified
-- by the plugin before generating "evidence" that uses this function.
evidenceHasField :: forall f r a.
     Int       -- ^ Field index
  -> FieldName -- ^ Field name
  -> Record f r
  -> (a -> Record f r, a)
evidenceHasField i n r@Record{..} = (
      \x -> r { recordDiff = Diff.set (i, n) (co' x) recordDiff }
    , co $ Diff.get (i, n) recordDiff recordCanon
    )
  where
    co  :: f Any -> a
    co' :: a -> f Any

    co =  noInlineUnsafeCo
    co' = noInlineUnsafeCo

{-------------------------------------------------------------------------------
  Simple evidence
-------------------------------------------------------------------------------}

evidenceAllFields   :: [Dict c Any]        -> DictAllFields r c
evidenceKnownFields :: [FieldMetadata Any] -> DictKnownFields r
evidenceIsomorphic  :: [Int]               -> DictIsomorphic r r'
evidenceKnownHash   :: Int                 -> DictKnownHash s

evidenceAllFields   x _ _ = Vector.fromList x
evidenceKnownFields x _   = x
evidenceIsomorphic  x _ _ = x
evidenceKnownHash   x _   = x
