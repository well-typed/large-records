{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interop with @generics-sop@ generics
module Data.Record.Generic.SOP (
    Fields
    -- | Translate between SOP representation and large-records representation
  , FromRep(..)
  , npToRep
  , npFromRep
  ) where

import Data.Bifunctor
import Data.Kind (Type, Constraint)
import Data.Proxy
import Data.Type.Equality ((:~:)(..))
import Generics.SOP (NP(..), Code, All, Shape(..))
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector  as V
import qualified Generics.SOP as SOP

import Data.Record.Generic

type family RecordFields (code :: [[Type]]) :: [Type] where
  RecordFields '[r] = r

type family Fields (a :: Type) :: [Type] where
  Fields a = RecordFields (Code a)

{-------------------------------------------------------------------------------
  Translate between SOP representation and large-records representation
-------------------------------------------------------------------------------}

-- | Translate from @SOP@ representation to @large-records@ representation
npToRep :: NP f (Fields a) -> Rep f a
npToRep = Rep . V.fromList . go
  where
    go :: NP f xs -> [f Any]
    go Nil       = []
    go (x :* xs) = unsafeCoerce x : go xs

data FromRep (c :: Type -> Constraint) (f :: Type -> Type) a where
  -- | The 'Rep' had exactly the right number of fields
  FromRepExact :: NP f (Fields a) -> FromRep c f a

  -- | The 'Rep' had too many fields
  FromRepTooMany :: NP f (Fields a) -> [f Any] -> FromRep c f a

  -- | The 'Rep' had too few fields
  --
  -- In this case the fields will be a prefix of @Fields a@, but instead of
  -- expressing that directly, we express it in a more convenient way: we
  -- promise that @c@ must hold for all of the fields.
  FromRepTooFew :: All c xs => NP f xs -> FromRep c f a

-- | Translate from @large-records@ representation to @SOP@ representation
--
-- 'FromRep' distinguishes between the various cases:
--
-- * The 'Rep' has exactly the right number of fields.
-- * The 'Rep' has too few fields fields.
--   In this case, we will guarantee that the fields that /are/ there satisfy
--   @c@ (well, by means of @unsafeCoerce@, we have no way of validating that
--   the types are right).
-- * The 'Rep' has too many fields.
--   Since we have no type information at all for the extra fields, we will
--   just return them as undecorated @[f Any]@.
npFromRep ::
     forall c f a. All c (Fields a)
  => Proxy c -> Rep f a -> FromRep c f a
npFromRep _ (Rep v) =
    go (SOP.shape :: Shape (Fields a)) (V.toList v) $ \pf np ->
      case pf of
        (Just (Refl, []))       -> FromRepExact   np
        (Just (Refl, leftover)) -> FromRepTooMany np leftover
        Nothing                 -> FromRepTooFew  np
  where
    go :: All c ys
       => Shape ys
       -> [f Any]
       -> (forall xs. All c xs => Maybe (xs :~: ys, [f Any]) -> NP f xs -> r)
       -> r
    go ShapeNil        xs     k = k (Just (Refl, xs)) Nil
    go (ShapeCons _)   []     k = k Nothing           Nil
    go s@(ShapeCons _) (x:xs) k = goCons s x xs k

    goCons ::
         forall r y ys. (c y, All c ys)
      => Shape (y ': ys)
      -> f Any
      -> [f Any]
      -> (forall xs. All c xs => Maybe (xs :~: y ': ys, [f Any]) -> NP f xs -> r)
      -> r
    goCons (ShapeCons s) x xs k =
        go s xs $ \pf np ->
          k (first (\Refl -> Refl) <$> pf) ((unsafeCoerce x :: f y) :* np)
