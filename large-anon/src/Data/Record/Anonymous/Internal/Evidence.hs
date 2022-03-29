{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Data.Kind
import Data.Record.Generic (FieldMetadata)
import Data.SOP.Dict
import GHC.Exts (Any)
import GHC.TypeLits (Symbol)

import qualified Data.Vector as Vector

import Data.Record.Anonymous.Internal.Row.FieldName (FieldName(..), DictKnownHash)
import Data.Record.Anonymous.Internal.Record
import Data.Record.Anonymous.Internal.Row

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
evidenceHasField i n r = (
      \x -> unsafeSetField i n x r
    ,       unsafeGetField i n   r
    )

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

