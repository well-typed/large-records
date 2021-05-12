{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Test.Record.Sanity.Transforms.Interpret.LR (
    -- * Normalization
    normalize
  , denormalize
    -- * Safe versions
  , HasNormalForm
  , safe_normalize
  , safe_denormalize
  ) where

import Data.Coerce
import Data.Kind
import Data.Maybe (fromJust)
import GHC.TypeLits (Symbol)
import Unsafe.Coerce (unsafeCoerce)

import Data.Record.Generic

import qualified Data.SOP                as SOP
import qualified Data.Record.Generic.SOP as LR_SOP

import Test.Record.Sanity.Transforms.Interpret

{-------------------------------------------------------------------------------
  Normalization
-------------------------------------------------------------------------------}

-- TODO: Can we make this safe (@HasNormalForm@) without going through SOP?
normalize ::
     Proxy d
  -> Proxy (x f)
  -> Rep I (x f) -> Rep (Interpret d f) (x Uninterpreted)
normalize _ _ = unsafeCoerce

denormalize ::
     Proxy d
  -> Proxy (x f)
  -> Rep (Interpret d f) (x Uninterpreted) -> Rep I (x f)
denormalize _ _ = unsafeCoerce

{-------------------------------------------------------------------------------
  Safe versions
-------------------------------------------------------------------------------}

class (
    LR_SOP.IsField field1
  , LR_SOP.IsField field2
  , Coercible (i1 (LR_SOP.FieldType field1)) (i2 (LR_SOP.FieldType field2))
  ) => CoerceField i1 i2 (field1 :: (Symbol, Type)) (field2 :: (Symbol, Type))
instance (
    LR_SOP.IsField field1
  , LR_SOP.IsField field2
  , Coercible (i1 (LR_SOP.FieldType field1)) (i2 (LR_SOP.FieldType field2))
  ) => CoerceField i1 i2 (field1 :: (Symbol, Type)) (field2 :: (Symbol, Type))

type HasNormalForm d x f = (
    SOP.AllZip (CoerceField I (Interpret d f)) (MetadataOf (x f)) (MetadataOf (x Uninterpreted))
  , SOP.AllZip (CoerceField (Interpret d f) I) (MetadataOf (x Uninterpreted)) (MetadataOf (x f))
  )

safe_normalize :: forall d f x.
     HasNormalForm d x f
  => Proxy d
  -> Proxy (x f)
  -> Rep I (x f) -> Rep (Interpret d f) (x Uninterpreted)
safe_normalize _ _ =
      LR_SOP.fromSOP
    . SOP.htrans (Proxy @(CoerceField I (Interpret d f))) (\(LR_SOP.Field x) -> LR_SOP.Field (coerce x))
    . fromJust
    . LR_SOP.toSOP

safe_denormalize :: forall d f x.
     HasNormalForm d x f
  => Proxy d
  -> Proxy (x f)
  -> Rep (Interpret d f) (x Uninterpreted) -> Rep I (x f)
safe_denormalize _ _ =
      LR_SOP.fromSOP
    . SOP.htrans (Proxy @(CoerceField (Interpret d f) I)) (\(LR_SOP.Field x) -> LR_SOP.Field (coerce x))
    . fromJust
    . LR_SOP.toSOP
