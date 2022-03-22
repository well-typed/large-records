{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE ExplicitNamespaces      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE OverloadedLabels        #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

-- | Model for records
--
-- 'NP' from @sop-core@ forms the basis for our model, along with a choice of
-- shape (zero, one, or two fields).
--
-- Intended for qualified import.
--
-- > import Test.Prop.Model (ModelRecord(..), ModelFields(..))
-- > import qualified Test.Prop.Model as Model
module Test.Prop.Record.Model (
    -- * Model proper
    ModelFields(..)
  , Types
  , ModelRecord(..)
    -- * Constraints
  , ModelSatisfies
  , satisfyAll
    -- * Conversion to/from 'Record'
  , toRecord
  , fromRecord
  , toRecordOfDicts
    -- * Combinators
    -- ** "Functor"
  , map
  , mapM
  , cmap
  , cmapM
    -- ** Zipping
  , zip
  , zipWith
  , zipWithM
  , czipWith
  , czipWithM
    -- ** "Foldable"
  , collapse
    -- ** "Traversable"
  , sequenceA
    -- ** "Applicative"
  , pure
  , cpure
  , ap
  ) where

import Prelude hiding (map, mapM, zip, zipWith, sequenceA, pure)

import Data.Functor.Product
import Data.Kind
import Data.Proxy
import Data.SOP (NP(..), type (-.->)(..), SListI, All)
import Data.SOP.BasicFunctors
import GHC.TypeLits

import qualified Data.SOP as SOP

import Data.Record.Anonymous.Advanced (Record)
import qualified Data.Record.Anonymous.Advanced as Anon

{-------------------------------------------------------------------------------
  Model proper
-------------------------------------------------------------------------------}

-- | Shapes of the different kinds of records we want to test
--
-- We want to test
--
-- * Records of different size (0, 1, or 2 fields)
-- * Fields ordered alphabetically or not
--   (for tests where order of processing matters)
--
-- TODO: Once we have support for /dropping/ fields, we should also add some
-- cases with duplicate fields here. We currently cannot, since we cannot define
-- 'fromRecord' for such records.
data ModelFields :: [(Symbol, Type)] -> Type where
  MF0  :: ModelFields '[                            ]
  MF1  :: ModelFields '[               '("b", Bool) ]
  MF2  :: ModelFields '[ '("a", Word), '("b", Bool) ]
  MF2' :: ModelFields '[ '("b", Word), '("a", Bool) ]

deriving instance Show (ModelFields xs)
deriving instance Eq   (ModelFields xs)

type family Types (fields :: [(Symbol, Type)]) :: [Type] where
  Types '[]             = '[]
  Types ('(_, t) ': ts) = t ': Types ts

data ModelRecord f r = MR (NP f (Types r))

deriving instance Show (NP f (Types r)) => Show (ModelRecord f r)
deriving instance Eq   (NP f (Types r)) => Eq   (ModelRecord f r)

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

class    (c Word, c Bool) => ModelSatisfies c
instance (c Word, c Bool) => ModelSatisfies c

satisfyAll ::
     ModelSatisfies c
  => Proxy c
  -> ModelFields r
  -> (All c (Types r) => a)
  -> a
satisfyAll _ MF0  k = k
satisfyAll _ MF1  k = k
satisfyAll _ MF2  k = k
satisfyAll _ MF2' k = k

{-------------------------------------------------------------------------------
  Conversion from/to model
-------------------------------------------------------------------------------}

toRecord :: ModelFields xs -> ModelRecord f xs -> Record f xs
toRecord MF0 (MR Nil) =
      Anon.empty
toRecord MF1 (MR (b :* Nil)) =
      Anon.insert #b b
    $ Anon.empty
toRecord MF2 (MR (a :* b :* Nil)) =
      Anon.insert #a a
    $ Anon.insert #b b
    $ Anon.empty
toRecord MF2' (MR (b :* a :* Nil)) =
      Anon.insert #b b
    $ Anon.insert #a a
    $ Anon.empty

fromRecord :: ModelFields xs -> Record f xs -> ModelRecord f xs
fromRecord MF0 _r =
    MR Nil
fromRecord MF1 r =
    MR (Anon.get #b r :* Nil)
fromRecord MF2 r =
    MR (Anon.get #a r :* Anon.get #b r :* Nil)
fromRecord MF2' r =
    MR (Anon.get #b r :* Anon.get #a r :* Nil)

toRecordOfDicts ::
     ModelSatisfies c
  => Proxy c
  -> ModelFields r
  -> (Anon.AllFields r c => a)
  -> a
toRecordOfDicts _ MF0  k = k
toRecordOfDicts _ MF1  k = k
toRecordOfDicts _ MF2  k = k
toRecordOfDicts _ MF2' k = k

{-------------------------------------------------------------------------------
  Simple combinators
-------------------------------------------------------------------------------}

map ::
     SListI (Types r)
  => (forall x. f x -> g x) -> ModelRecord f r -> ModelRecord g r
map f (MR np) = MR (SOP.hmap f np)

mapM ::
     SListI (Types r)
  => Applicative m
  => (forall x. f x -> m (g x))
  -> ModelRecord f r -> m (ModelRecord g r)
mapM f (MR np) = MR <$> SOP.htraverse' f np

zip ::
     SListI (Types r)
  => ModelRecord f r -> ModelRecord g r -> ModelRecord (Product f g) r
zip = zipWith Pair

zipWith ::
     SListI (Types r)
  => (forall x. f x -> g x -> h x)
  -> ModelRecord f r -> ModelRecord g r -> ModelRecord h r
zipWith f (MR np) (MR np') = MR (SOP.hzipWith f np np')

zipWithM :: forall m f g h r.
     SListI (Types r)
  => Applicative m
  => (forall x. f x -> g x -> m (h x))
  -> ModelRecord f r -> ModelRecord g r -> m (ModelRecord h r)
zipWithM f (MR np) (MR np') =
    fmap MR $ SOP.hsequence' $ SOP.hzipWith f' np np'
  where
    f' :: forall x. f x -> g x -> (m :.: h) x
    f' x y = Comp $ f x y

collapse :: SListI (Types r) => ModelRecord (K a) r -> [a]
collapse (MR np) = SOP.hcollapse np

sequenceA ::
     SListI (Types r)
  => Applicative m
  => ModelRecord (m :.: f) r -> m (ModelRecord f r)
sequenceA (MR np) = MR <$> SOP.hsequence' np

pure :: ModelFields r -> (forall x. f x) -> ModelRecord f r
pure MF0  f = MR (SOP.hpure f)
pure MF1  f = MR (SOP.hpure f)
pure MF2  f = MR (SOP.hpure f)
pure MF2' f = MR (SOP.hpure f)

ap ::
     SListI (Types r)
  => ModelRecord (f -.-> g) r -> ModelRecord f r -> ModelRecord g r
ap (MR np) (MR np') = MR $ SOP.hliftA2 apFn np np'

{-------------------------------------------------------------------------------
  Constrained combinators
-------------------------------------------------------------------------------}

cpure ::
     ModelSatisfies c
  => Proxy c
  -> ModelFields r
  -> (forall x. c x => f x)
  -> ModelRecord f r
cpure p MF0  f = MR (SOP.hcpure p f)
cpure p MF1  f = MR (SOP.hcpure p f)
cpure p MF2  f = MR (SOP.hcpure p f)
cpure p MF2' f = MR (SOP.hcpure p f)

cmap ::
     All c (Types r)
  => Proxy c
  -> (forall x. c x => f x -> g x)
  -> ModelRecord f r -> ModelRecord g r
cmap p f (MR np) = MR (SOP.hcmap p f np)

cmapM ::
     (Applicative m, All c (Types r))
  => Proxy c
  -> (forall x. c x => f x -> m (g x))
  -> ModelRecord f r -> m (ModelRecord g r)
cmapM p f (MR np) = fmap MR $ SOP.hctraverse' p f np

czipWith ::
     All c (Types r)
  => Proxy c
  -> (forall x. c x => f x -> g x -> h x)
  -> ModelRecord f r -> ModelRecord g r -> ModelRecord h r
czipWith p f (MR np) (MR np') = MR (SOP.hczipWith p f np np')

czipWithM :: forall m c f g h r.
     (Applicative m, All c (Types r))
  => Proxy c
  -> (forall x. c x => f x -> g x -> m (h x))
  -> ModelRecord f r -> ModelRecord g r -> m (ModelRecord h r)
czipWithM p f (MR np) (MR np') =
    fmap MR $ SOP.hsequence' $ SOP.hczipWith p f' np np'
  where
    f' :: forall x. c x => f x -> g x -> (m :.: h) x
    f' x y = Comp $ f x y
