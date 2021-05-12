{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Record.Sanity.Transforms.Interpret.SOP (
    -- * Normalization
    HasNormalForm
  , normalize
  , denormalize
    -- * Convenience
  , normalizedFrom
  , toDenormalized
  , normalizedProductFrom
  , productToDenormalized
  ) where

import Generics.SOP

import Test.Record.Sanity.Transforms.Interpret

{-------------------------------------------------------------------------------
  Normalization
-------------------------------------------------------------------------------}

-- | Terms with a normal form
--
-- TODO: Ideally this would be forall f, rather than for a specific f;
-- also, the second direction should be implied by the first..?
type HasNormalForm d x f = (
    AllZip2 (LiftedCoercible I (Interpret d f)) (Code (x f)) (Code (x Uninterpreted))
  , AllZip2 (LiftedCoercible (Interpret d f) I) (Code (x Uninterpreted)) (Code (x f))
  )

normalize ::
     HasNormalForm d x f
  => Proxy d
  -> Proxy (x f)
  -> SOP I (Code (x f)) -> SOP (Interpret d f) (Code (x Uninterpreted))
normalize _ _ = hfromI

denormalize ::
     HasNormalForm d x f
  => Proxy d
  -> Proxy (x f)
  -> SOP (Interpret d f) (Code (x Uninterpreted)) -> SOP I (Code (x f))
denormalize _ _ = htoI

{-------------------------------------------------------------------------------
  Convenience
-------------------------------------------------------------------------------}

normalizedFrom :: forall d x f.
     (Generic (x f), HasNormalForm d x f)
  => x f -> SOP (Interpret d f) (Code (x Uninterpreted))
normalizedFrom = normalize (Proxy @d) (Proxy @(x f)) . from

toDenormalized :: forall d x f.
     (Generic (x f), HasNormalForm d x f)
  => SOP (Interpret d f) (Code (x Uninterpreted)) -> x f
toDenormalized = to . denormalize (Proxy @d) (Proxy @(x f))

normalizedProductFrom :: forall d x f ys.
     (Generic (x f), HasNormalForm d x f, Code (x Uninterpreted) ~ '[ys])
  => x f -> NP (Interpret d f) ys
normalizedProductFrom = toProduct . normalizedFrom

productToDenormalized :: forall d x f ys.
     (Generic (x f), HasNormalForm d x f, Code (x Uninterpreted) ~ '[ys])
  => NP (Interpret d f) ys -> x f
productToDenormalized = toDenormalized . fromProduct

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

toProduct :: xs ~ '[ys] => SOP f xs -> NP f ys
toProduct (SOP (Z xs)) = xs
toProduct (SOP (S xs)) = case xs of {}

fromProduct :: xs ~ '[ys] => NP f ys -> SOP f xs
fromProduct = SOP . Z
