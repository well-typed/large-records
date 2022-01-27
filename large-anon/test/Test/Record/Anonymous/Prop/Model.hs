{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Model for records
--
-- 'NP' from @sop-core@ forms the basis for our model, along with a choice of
-- shape (zero, one, or two fields).
--
-- Intended for qualified import.
--
-- > import Test.Record.Anonymous.Prop.Model (ModelRecord(..), ModelFields(..))
-- > import qualified Test.Record.Anonymous.Prop.Model as Model
module Test.Record.Anonymous.Prop.Model (
    -- * Model proper
    ModelFields(..)
  , Types
  , ModelRecord(..)
    -- * Conversion to/from 'Record'
  , toRecord
  , fromRecord
    -- * Combinators
    -- ** "Functor"
  , map
  , mapM
    -- ** Zipping
  , zip
  , zipWith
  , zipWithM
    -- ** "Foldable"
  , collapse
    -- ** "Traversable"
  , sequenceA
    -- ** "Applicative"
  , pure
  , ap
  ) where

import Prelude hiding (map, mapM, zip, zipWith, sequenceA, pure)

import Data.Functor.Product
import Data.Kind
import Data.SOP (NP(..), type (-.->)(..), SListI)
import Data.SOP.BasicFunctors
import GHC.TypeLits

import qualified Data.SOP as SOP

import Data.Record.Anonymous (Record)
import qualified Data.Record.Anonymous as Anon

{-------------------------------------------------------------------------------
  Model proper
-------------------------------------------------------------------------------}

data ModelFields :: [(Symbol, Type)] -> Type where
  MF0 :: ModelFields '[                           ]
  MF1 :: ModelFields '[              '("b", Bool) ]
  MF2 :: ModelFields '[ '("a", Int), '("b", Bool) ]

deriving instance Show (ModelFields xs)
deriving instance Eq   (ModelFields xs)

type family Types (fields :: [(Symbol, Type)]) :: [Type] where
  Types '[]             = '[]
  Types ('(_, t) ': ts) = t ': Types ts

data ModelRecord f r = MR (NP f (Types r))

deriving instance Show (NP f (Types r)) => Show (ModelRecord f r)
deriving instance Eq   (NP f (Types r)) => Eq   (ModelRecord f r)

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

fromRecord :: ModelFields xs -> Record f xs -> ModelRecord f xs
fromRecord MF0 _r =
    MR Nil
fromRecord MF1 r =
    MR (Anon.get #b r :* Nil)
fromRecord MF2 r =
    MR (Anon.get #a r :* Anon.get #b r :* Nil)

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
pure MF0 _ = MR Nil
pure MF1 f = MR (f :* Nil)
pure MF2 f = MR (f :* f :* Nil)

ap ::
     SListI (Types r)
  => ModelRecord (f -.-> g) r -> ModelRecord f r -> ModelRecord g r
ap (MR np) (MR np') = MR $ SOP.hliftA2 apFn np np'

