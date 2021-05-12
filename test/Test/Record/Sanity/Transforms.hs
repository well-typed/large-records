{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wno-unused-binds #-}

-- | Proof of concept demonstration of how to simulate @htrans@
module Test.Record.Sanity.Transforms (
  ) where

import Data.Kind
import Data.Proxy
import Generics.SOP (Code, SOP)
import Data.SOP.BasicFunctors

import Data.Record.TH

import qualified Data.Record.Generic     as LR
import qualified Data.Record.Generic.Rep as LR
import qualified Data.SOP                as SOP
import qualified Generics.SOP            as SOP
import qualified GHC.Generics            as GHC

import Test.Record.Sanity.Transforms.Interpret
import Test.Record.Sanity.Transforms.MiniBeam

import qualified Test.Record.Sanity.Transforms.Interpret.LR  as LR
import qualified Test.Record.Sanity.Transforms.Interpret.SOP as SOP

{-------------------------------------------------------------------------------
  Simple SOP example, nothing fancy
-------------------------------------------------------------------------------}

data A (f :: Type -> Type) = A {
      aI :: f Int
    , aB :: f Bool
    , aS :: String
    }
  deriving stock    (GHC.Generic)
  deriving anyclass (SOP.Generic)

deriving instance (Show (f Int), Show (f Bool)) => Show (A f)

class CanInject x y where
  inject :: x -> y

instance CanInject (I x) (Maybe x) where
  inject (I x) = Just x

instance CanInject String String where
  inject = id

gjust :: forall x.
     ( SOP.Generic (x I)
     , SOP.Generic (x Maybe)
     , SOP.AllZip2 CanInject (Code (x I)) (Code (x Maybe))
     )
  => x I -> x Maybe
gjust =
    SOP.to . aux . SOP.from
  where
    aux :: SOP I (Code (x I)) -> SOP I (Code (x Maybe))
    aux = SOP.htrans (Proxy @CanInject) (fmap inject)

justA :: A I -> A Maybe
justA = gjust

{-------------------------------------------------------------------------------
  Intermediate step: using SOP, but with htrans at the /edges/
-------------------------------------------------------------------------------}

type instance Interpreted Maybe f (Uninterpreted x) = f x
type instance Interpreted Maybe f String = String

class CanInjectInterpreted f g a where
  injectInterpreted :: Interpret Maybe f a -> Interpret Maybe g a

instance CanInjectInterpreted I Maybe (Uninterpreted a) where
  injectInterpreted = liftInterpreted $ \(I x) -> Just x

instance CanInjectInterpreted I Maybe String where
  injectInterpreted = liftInterpreted $ id

gjustSOP :: forall x f g.
     ( SOP.Generic (x f)
     , SOP.Generic (x g)
     , SOP.HasNormalForm Maybe x f
     , SOP.HasNormalForm Maybe x g
     , SOP.All2 (CanInjectInterpreted f g) (Code (x Uninterpreted))
     )
  => x f -> x g
gjustSOP =
      SOP.toDenormalized
    . SOP.hcmap (Proxy @(CanInjectInterpreted f g)) injectInterpreted
    . SOP.normalizedFrom

justA' :: A I -> A Maybe
justA' = gjustSOP

{-------------------------------------------------------------------------------
  Using LR rather than SOP
-------------------------------------------------------------------------------}

largeRecord (defaultLazyOptions { generatePatternSynonym = True }) [d|
    data B (f :: Type -> Type) = B {
          bI :: f Int
        , bB :: f Bool
        , bS :: String
        }
      deriving (Show)
  |]

gjustLR :: forall x (f :: Type -> Type) (g :: Type -> Type).
     ( LR.Generic (x f)
     , LR.Generic (x g)
     , LR.Generic (x Uninterpreted)
     , LR.Constraints (x Uninterpreted) (CanInjectInterpreted f g)
     )
  => x f -> x g
gjustLR =
      LR.to
    . LR.denormalize (Proxy @Maybe) (Proxy @(x g))
    . LR.cmap (Proxy @(CanInjectInterpreted f g)) injectInterpreted
    . LR.normalize (Proxy @Maybe) (Proxy @(x f))
    . LR.from

safe_gjustLR :: forall x (f :: Type -> Type) (g :: Type -> Type).
     ( LR.Generic (x f)
     , LR.Generic (x g)
     , LR.Generic (x Uninterpreted)
     , LR.Constraints (x Uninterpreted) (CanInjectInterpreted f g)
     , LR.HasNormalForm Maybe x f
     , LR.HasNormalForm Maybe x g
     )
  => x f -> x g
safe_gjustLR =
      LR.to
    . LR.safe_denormalize (Proxy @Maybe) (Proxy @(x g))
    . LR.cmap (Proxy @(CanInjectInterpreted f g)) injectInterpreted
    . LR.safe_normalize (Proxy @Maybe) (Proxy @(x f))
    . LR.from

justB :: B I -> B Maybe
justB = gjustLR

safe_justB :: B I -> B Maybe
safe_justB = safe_gjustLR

{-------------------------------------------------------------------------------
  Beam example
-------------------------------------------------------------------------------}

data TableA f = TableA {
      taFieldI :: PrimaryKey TableA f
    , taFieldB :: Columnar f Bool
    , taFieldM :: TableB f
    }
  deriving stock    (GHC.Generic)
  deriving anyclass (SOP.Generic)

data TableB f = TableB {
      tbFieldC :: Columnar f Char
    }
  deriving stock    (GHC.Generic)
  deriving anyclass (SOP.Generic)

instance Beamable TableA where
  zipBeamFieldsM = gzipBeamSOP

instance Beamable TableB where
  zipBeamFieldsM = gzipBeamSOP

instance Beamable (PrimaryKey TableA) where
  zipBeamFieldsM = gzipBeamSOP

instance Table TableA where
  data PrimaryKey TableA f = PrimA (Columnar f Int)
    deriving stock    (GHC.Generic)
    deriving anyclass (SOP.Generic)

  primaryKey = taFieldI

type instance Interpreted Columnar' f (table Uninterpreted) = table f
type instance Interpreted Columnar' f (Uninterpreted x)     = Columnar' f x

class ZipInterpreted a where
  zipInterpreted ::
       Applicative m
    => (forall x. Columnar' f x -> Columnar' g x -> m (Columnar' h x))
    -> Interpret Columnar' f a
    -> Interpret Columnar' g a
    -> m (Interpret Columnar' h a)

instance Beamable table => ZipInterpreted (table Uninterpreted) where
  zipInterpreted f = liftInterpretedA2 $ zipBeamFieldsM f

instance ZipInterpreted (Uninterpreted x) where
  zipInterpreted = liftInterpretedA2

gzipBeamSOP :: forall m table fields f g h.
     ( Applicative m
     , SOP.Generic (table f)
     , SOP.Generic (table g)
     , SOP.Generic (table h)
     , SOP.HasNormalForm Columnar' table f
     , SOP.HasNormalForm Columnar' table g
     , SOP.HasNormalForm Columnar' table h
     , Code (table Uninterpreted) ~ '[fields]
     , SOP.All ZipInterpreted fields
     )
  => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
  -> table f -> table g -> m (table h)
gzipBeamSOP f a b =
    fmap SOP.productToDenormalized $
      SOP.hsequence' $
        SOP.hcliftA2
          (Proxy @ZipInterpreted)
          (\fa ga -> Comp $ zipInterpreted f fa ga)
          (SOP.normalizedProductFrom a)
          (SOP.normalizedProductFrom b)

{-------------------------------------------------------------------------------
  In LR
-------------------------------------------------------------------------------}

largeRecord (defaultLazyOptions { generatePatternSynonym = True }) [d|
    data TableC (f :: Type -> Type) = TableC {
          tcFieldI :: PrimaryKey TableC f
        , tcFieldB :: Columnar f Bool
        , tcFieldM :: TableB f
        }

    data TableD (f :: Type -> Type) = TableD {
          tdFieldC :: Columnar f Char
        }
  |]

instance Beamable TableC where
  zipBeamFieldsM = safe_gzipBeamLR --  gzipBeamLR also fine, of course

instance Beamable TableD where
  zipBeamFieldsM = safe_gzipBeamLR

instance Beamable (PrimaryKey TableC) where
  zipBeamFieldsM = gzipBeamSOP

instance Table TableC where
  data PrimaryKey TableC f = PrimC (Columnar f Int)
    deriving stock    (GHC.Generic)
    deriving anyclass (SOP.Generic)

  primaryKey = tcFieldI

gzipBeamLR :: forall m table f g h.
     ( Applicative m
     , LR.Generic (table f)
     , LR.Generic (table g)
     , LR.Generic (table h)
     , LR.Generic (table Uninterpreted)
     , LR.Constraints (table Uninterpreted) ZipInterpreted
     )
  => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
  -> table f -> table g -> m (table h)
gzipBeamLR f a b =
    fmap (LR.to . LR.denormalize (Proxy @Columnar') (Proxy @(table h))) $
      LR.czipWithM
        (Proxy @ZipInterpreted)
        (zipInterpreted f)
        (LR.normalize (Proxy @Columnar') (Proxy @(table f)) (LR.from a))
        (LR.normalize (Proxy @Columnar') (Proxy @(table g)) (LR.from b))

safe_gzipBeamLR :: forall m table f g h.
     ( Applicative m
     , LR.Generic (table f)
     , LR.Generic (table g)
     , LR.Generic (table h)
     , LR.Generic (table Uninterpreted)
     , LR.Constraints (table Uninterpreted) ZipInterpreted
     , LR.HasNormalForm Columnar' table f
     , LR.HasNormalForm Columnar' table g
     , LR.HasNormalForm Columnar' table h
     )
  => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
  -> table f -> table g -> m (table h)
safe_gzipBeamLR f a b =
    fmap (LR.to . LR.safe_denormalize (Proxy @Columnar') (Proxy @(table h))) $
      LR.czipWithM
        (Proxy @ZipInterpreted)
        (zipInterpreted f)
        (LR.safe_normalize (Proxy @Columnar') (Proxy @(table f)) (LR.from a))
        (LR.safe_normalize (Proxy @Columnar') (Proxy @(table g)) (LR.from b))
