{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | van Laarhoven lenses for large records.
-- The type synonym
--
-- @
--   type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
-- @
-- Appears below, however it is not exported to avoid conflicts with other
-- libraries defining equivalent synonyms.
module Data.Record.Generic.Lens.VL (
    -- * Lenses for records
    SimpleRecordLens(..)
  , HKRecordLens(..)
  , RegularRecordLens(..)
  , lensesForSimpleRecord
  , lensesForHKRecord
  , lensesForRegularRecord
    -- * Regular records
  , RegularField(..)
  , IsRegularField(..)
    -- * Lenses into 'Rep'
  , RepLens(..)
  , repLenses
    -- * General purpose lenses
  , genericLens
  , normalForm1Lens
  , interpretedLens
  , standardInterpretationLens
  ) where

import Data.Kind

import Data.Record.Generic
import Data.Record.Generic.Transform

import qualified Data.Record.Generic.Rep as Rep

-- | The standard van Laarhoven representation for a monomorphic lens
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

{-------------------------------------------------------------------------------
  Simple records (in contrast to higher-kinded records, see below)
-------------------------------------------------------------------------------}

data SimpleRecordLens a b where
  SimpleRecordLens :: Lens' a b -> SimpleRecordLens a b

-- | Construct lenses for each field in the record
--
-- NOTE: This is of limited use since we cannot pattern match on the resulting
-- 'Rep' in any meaningful way. It is possible to go through the SOP adapter,
-- but if we do, we incur quadratic cost again.
--
-- We can do better for higher-kinded records, and better still for regular
-- higher-kinded records. See 'lensesForHKRecord' and 'lensesForRegularRecord'.
lensesForSimpleRecord :: forall a. Generic a => Rep (SimpleRecordLens a) a
lensesForSimpleRecord =
    Rep.map (\(RepLens l) -> SimpleRecordLens $ \f -> aux l f) repLenses
  where
    aux :: Lens' (Rep I a) (I x) -> Lens' a x
    aux l f a = to <$> l (\(I x) -> I <$> f x) (from a)

{-------------------------------------------------------------------------------
  Higher-kinded records (records with a functor parameter)
-------------------------------------------------------------------------------}

-- | Lens for higher-kinded record
--
-- See 'lensesForHKRecord' for details.
data HKRecordLens d (f :: Type -> Type) tbl x where
  HKRecordLens :: Lens' (tbl f) (Interpret (d f) x) -> HKRecordLens d f tbl x

-- | Lenses for higher-kinded records
--
-- NOTE: The lenses constructed by this function are primarily intended for
-- further processing, either by 'lensesForRegularRecord' or using application
-- specific logic. Details below.
--
-- Suppose we have a record @tbl f@ which is indexed by a functor @f@, and we
-- want to construct lenses from @tbl f@ to each field in the record. Using the
-- @Transform@ infrastructure, we can construct a lens
--
-- > tbl f ~~> Rep I (tbl f) ~~> Rep (Interpret (d f)) (tbl Uninterpreted)
--
-- Using 'repLenses' we can construct a lens of type
--
-- > Rep (Interpret (d f)) (tbl Uninterpreted) ~~> Interpret (d f) x
--
-- for every field of type @x@. Putting these two together gives us a lens
--
-- > tbl f ~~> Interpret (d f) x
--
-- for every field in @tbl Uninterpreted@. We cannot simplify this, because we
-- do not know anything about the shape of @x@; specifically, it might not be
-- equal to @Uninterpreted x'@ for some @x'@, and hence we cannot simplify the
-- target type of the lens. We can do better for records with regular fields;
-- see 'lensesForRegularRecord'.
lensesForHKRecord :: forall d tbl f.
     ( Generic (tbl f)
     , Generic (tbl Uninterpreted)
     , HasNormalForm (d f) (tbl f) (tbl Uninterpreted)
     )
  => Proxy d -> Rep (HKRecordLens d f tbl) (tbl Uninterpreted)
lensesForHKRecord d = Rep.map aux fromRepLenses
  where
    fromRepLenses :: Rep (RepLens (Interpret (d f)) (tbl Uninterpreted)) (tbl Uninterpreted)
    fromRepLenses = repLenses

    aux :: forall x. RepLens (Interpret (d f)) (tbl Uninterpreted) x -> HKRecordLens d f tbl x
    aux (RepLens l) = HKRecordLens $
          genericLens
        . normalForm1Lens d
        . l

{-------------------------------------------------------------------------------
  Regular records
-------------------------------------------------------------------------------}

-- | Proof that @x@ is a regular field
--
-- See 'IsRegularField'
data RegularField f x where
  RegularField :: RegularField f (f x)

-- | Regular record fields
--
-- For a higher-kinded record @tbl f@, parameterized over some functor @f@,
-- we say that the fields are /regular/ iff every field has the form @f x@
-- for some @x@.
class IsRegularField f x where
  isRegularField :: Proxy (f x) -> RegularField f x

instance IsRegularField f (f x) where
  isRegularField _ = RegularField

{-------------------------------------------------------------------------------
  Lenses into regular records
-------------------------------------------------------------------------------}

-- | Lens into a regular record
--
-- See 'lensesForRegularRecord'
data RegularRecordLens tbl f x where
  RegularRecordLens :: Lens' (tbl f) (f x) -> RegularRecordLens tbl f x

-- | Lenses into higher-kinded records with regular fields
--
-- We can use 'lensesForHKRecord' to construct a 'Rep' of lenses into a higher-kinded
-- record. If in addition the record is regular, we can use the record type
-- /itself/ to store all the lenses.
lensesForRegularRecord :: forall d tbl f.
     ( Generic (tbl (RegularRecordLens tbl f))
     , Generic (tbl Uninterpreted)
     , Generic (tbl f)
     , HasNormalForm (d (RegularRecordLens tbl f)) (tbl (RegularRecordLens tbl f)) (tbl Uninterpreted)
     , HasNormalForm (d f) (tbl f) (tbl Uninterpreted)
     , Constraints (tbl Uninterpreted) (IsRegularField Uninterpreted)
     , StandardInterpretation d (RegularRecordLens tbl f)
     , StandardInterpretation d f
     )
  => Proxy d -> tbl (RegularRecordLens tbl f)
lensesForRegularRecord d = to . denormalize1 d $
    Rep.cmap
      (Proxy @(IsRegularField Uninterpreted))
      aux
      (lensesForHKRecord d)
  where
    aux :: forall x.
         IsRegularField Uninterpreted x
      => HKRecordLens d f tbl x
      -> Interpret (d (RegularRecordLens tbl f)) x
    aux (HKRecordLens l) =
        case isRegularField (Proxy @(Uninterpreted x)) of
          RegularField -> toStandardInterpretation d $ RegularRecordLens $
             l . standardInterpretationLens d

{-------------------------------------------------------------------------------
  Lenses into 'Rep'
-------------------------------------------------------------------------------}

data RepLens f a x where
  RepLens :: Lens' (Rep f a) (f x) -> RepLens f a x

repLenses :: Generic a => Rep (RepLens f a) a
repLenses = Rep.map aux Rep.allIndices
  where
    aux :: Rep.Index a x -> RepLens f a x
    aux ix = RepLens $ Rep.updateAtIndex ix

{-------------------------------------------------------------------------------
  General purpose lenses
-------------------------------------------------------------------------------}

genericLens :: Generic a => Lens' a (Rep I a)
genericLens f a = to <$> f (from a)

normalForm1Lens ::
     HasNormalForm (d f) (x f) (x Uninterpreted)
  => Proxy d
  -> Lens' (Rep I (x f)) (Rep (Interpret (d f)) (x Uninterpreted))
normalForm1Lens p f a = denormalize1 p <$> f (normalize1 p a)

interpretedLens :: Lens' (Interpret d x) (Interpreted d x)
interpretedLens f (Interpret x) = Interpret <$> f x

standardInterpretationLens :: forall d f x.
     StandardInterpretation d f
  => Proxy d
  -> Lens' (Interpret (d f) (Uninterpreted x)) (f x)
standardInterpretationLens p f x =
    toStandardInterpretation p <$>
      f (fromStandardInterpretation p x)
