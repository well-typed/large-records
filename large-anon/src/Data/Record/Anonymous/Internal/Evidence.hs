{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- | Used by the plugin for evidence construction during constraint resolution
--
-- This module is exclusively used by the plugin; it is not used anywhere
-- inside the rest of the library: there are no imports of this module.
module Data.Record.Anonymous.Internal.Evidence (
    evidenceAllFields
  , evidenceHasField
  , evidenceKnownFields
  , evidenceKnownHash
  , evidenceProject
  ) where

import Data.Record.Generic (FieldMetadata)
import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)
import Data.SOP.Dict
import GHC.Exts (Any)

import qualified Data.Vector as Vector

import Data.Record.Anonymous.Internal.Row.FieldName (FieldName(..), DictKnownHash)
import Data.Record.Anonymous.Internal.Record
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Internal.Diff as Diff
import Data.Kind
import GHC.TypeLits (Symbol)

{-------------------------------------------------------------------------------
  'HasField'
-------------------------------------------------------------------------------}

-- | Evidence for 'HasField'
--
-- Precondition: the record must have the specified field with type @a@ (where
-- @a@ will be of the form @f a'@ for some @a'). This precondition is verified
-- by the plugin before generating "evidence" that uses this function.
evidenceHasField :: forall k (f :: k -> Type) (r :: Row k) a.
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

  We are explicit about kind arguments to make code generation a bit easier.
-------------------------------------------------------------------------------}

evidenceAllFields :: forall k (r :: Row k) (c :: k -> Constraint).
  [Dict c Any] -> DictAllFields k r c
evidenceAllFields x _ _ = Vector.fromList x

evidenceKnownFields :: forall k (r :: Row k).
  [FieldMetadata Any] -> DictKnownFields k r
evidenceKnownFields x _ = x

evidenceKnownHash :: forall (s :: Symbol).
  Int -> DictKnownHash s
evidenceKnownHash x _   = x

evidenceProject :: forall k (f :: k -> Type) (r :: Row k) (r' :: Row k).
  [Int] -> DictProject k f r r'
evidenceProject x _ _ _ = x

