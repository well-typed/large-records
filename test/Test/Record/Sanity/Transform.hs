{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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

module Test.Record.Sanity.Transform (tests) where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.SOP.BasicFunctors
import GHC.TypeLits

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.TH
import Data.Record.Generic
import Data.Record.Generic.Transform

import qualified Data.Record.Generic.Rep as Rep
import qualified Generics.SOP            as SOP

{-------------------------------------------------------------------------------
  Motivating example using SOP
-------------------------------------------------------------------------------}

class CanInject x y where
  inject :: x -> y

instance CanInject (I x) (Maybe x) where
  inject (I x) = Just x

instance CanInject String String where
  inject = id

_gjust_SOP :: forall x fields fields'.
     ( SOP.Generic (x I)
     , SOP.Generic (x Maybe)
     , SOP.IsProductType (x I)     fields
     , SOP.IsProductType (x Maybe) fields'
     , SOP.AllZip CanInject fields fields'
     )
  => x I -> x Maybe
_gjust_SOP =
    SOP.productTypeTo . aux . SOP.productTypeFrom
  where
    aux :: SOP.NP I fields -> SOP.NP I fields'
    aux = SOP.htrans (Proxy @CanInject) (fmap inject)

{-------------------------------------------------------------------------------
  Simple example
-------------------------------------------------------------------------------}

type instance Interpreted (DefaultInterpretation f) String = String

class InjectInterpreted f g a where
  injectInterpreted ::
       Interpret (DefaultInterpretation f) a
    -> Interpret (DefaultInterpretation g) a

instance InjectInterpreted I Maybe (Uninterpreted a) where
  injectInterpreted = liftInterpreted $ \(I x) -> Just x

instance InjectInterpreted I Maybe String where
  injectInterpreted = liftInterpreted $ id

largeRecord (defaultLazyOptions { generatePatternSynonym = GenPatSynonym }) [d|
    data A (f :: Type -> Type) = A {
          aI :: f Int
        , aB :: f Bool
        , aS :: String
        }
      deriving (Show, Eq)
  |]

-- | Generic injection, using LR generics
--
-- The type annotations are just to explain the flow, they are not required
-- for type inference.
gjust :: forall x (f :: Type -> Type) (g :: Type -> Type).
     ( Generic (x f)
     , Generic (x g)
     , Generic (x Uninterpreted)
     , Constraints (x Uninterpreted) (InjectInterpreted f g)
     , HasNormalForm (DefaultInterpretation f) (x f) (x Uninterpreted)
     , HasNormalForm (DefaultInterpretation g) (x g) (x Uninterpreted)
     )
  => x f -> x g
gjust =
      (to
         :: Rep I (x g)
         -> x g)
    . (denormalize1 (Proxy @DefaultInterpretation)
         :: Rep (Interpret (DefaultInterpretation g)) (x Uninterpreted)
         -> Rep I (x g))
    . (Rep.cmap (Proxy @(InjectInterpreted f g)) injectInterpreted
         :: Rep (Interpret (DefaultInterpretation f)) (x Uninterpreted)
         -> Rep (Interpret (DefaultInterpretation g)) (x Uninterpreted))
    . (normalize1 (Proxy @DefaultInterpretation)
         :: Rep I (x f)
         -> Rep (Interpret (DefaultInterpretation f)) (x Uninterpreted))
    . (from
         :: x f
         -> Rep I (x f))

justA :: A I -> A Maybe
justA = gjust

{-------------------------------------------------------------------------------
  Example with two variables
-------------------------------------------------------------------------------}

largeRecord (defaultLazyOptions { generatePatternSynonym = GenPatSynonym }) [d|
    data B (f :: Type -> Type) (g :: Type -> Type) = B {
          bI :: f Int
        , bB :: g Bool
        , bS :: String
        }
      deriving (Show, Eq)
  |]

data Skolem (n :: Nat) x

data DefInt2 (f :: Type -> Type) (g :: Type -> Type)

type instance Interpreted (DefInt2 f g) (Skolem 0 x) = f x
type instance Interpreted (DefInt2 f g) (Skolem 1 x) = g x
type instance Interpreted (DefInt2 f g) String       = String

class SwapInterpreted f g a where
  swapInterpreted ::
       Interpret (DefInt2 f g) a
    -> Interpret (DefInt2 g f) a

instance SwapInterpreted I Identity (Skolem 0 x) where
  swapInterpreted = liftInterpreted $ \(I x) -> Identity x

instance SwapInterpreted I Identity (Skolem 1 y) where
  swapInterpreted = liftInterpreted $ \(Identity x) -> I x

instance SwapInterpreted f g String where
  swapInterpreted = liftInterpreted $ id

gswap :: forall x (f :: Type -> Type) (g :: Type -> Type).
     ( Generic (x f g)
     , Generic (x g f)
     , Generic (x (Skolem 0) (Skolem 1))
     , Constraints (x (Skolem 0) (Skolem 1)) (SwapInterpreted f g)
     , HasNormalForm (DefInt2 f g) (x f g) (x (Skolem 0) (Skolem 1))
     , HasNormalForm (DefInt2 g f) (x g f) (x (Skolem 0) (Skolem 1))
     )
  => x f g -> x g f
gswap =
      to
    . denormalize (Proxy @(DefInt2 g f)) (Proxy @(x (Skolem 0) (Skolem 1)))
    . Rep.cmap (Proxy @(SwapInterpreted f g)) swapInterpreted
    . normalize (Proxy @(DefInt2 f g)) (Proxy @(x (Skolem 0) (Skolem 1)))
    . from

swapB :: B I Identity -> B Identity I
swapB = gswap

{-------------------------------------------------------------------------------
  Mini-beam
-------------------------------------------------------------------------------}

data Nullable (c :: Type -> Type) x

type family Columnar (f :: Type -> Type) x where
  Columnar Identity     x = x
  Columnar (Nullable c) x = Columnar c (Maybe x)
  Columnar f            x = f x

newtype Columnar' f a = Columnar' { getColumnar' :: Columnar f a }

class Beamable table where
  zipBeamFieldsM ::
       Applicative m
    => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
    -> table f -> table g -> m (table h)

class (Beamable table, Beamable (PrimaryKey table)) => Table table where
  data PrimaryKey table (f :: Type -> Type) :: Type

{-------------------------------------------------------------------------------
  Interpretation function for Beam
-------------------------------------------------------------------------------}

data BeamInterpretation (f :: Type -> Type)

type instance Interpreted (BeamInterpretation f) (table Uninterpreted) = table f
type instance Interpreted (BeamInterpretation f) (Uninterpreted x)     = Columnar f x

class ZipInterpreted a where
  zipInterpreted ::
       Applicative m
    => (forall x. Columnar' f x -> Columnar' g x -> m (Columnar' h x))
    -> Interpret (BeamInterpretation f) a
    -> Interpret (BeamInterpretation g) a
    -> m (Interpret (BeamInterpretation h) a)

instance Beamable table => ZipInterpreted (table Uninterpreted) where
  zipInterpreted f = liftInterpretedA2 $ zipBeamFieldsM f

instance ZipInterpreted (Uninterpreted x) where
  zipInterpreted f = liftInterpretedA2 $ applyColumnar' (Proxy @x) f

applyColumnar' :: forall m f g h x.
     Functor m
  => Proxy x
  -> (Columnar' f x -> Columnar' g x -> m (Columnar' h x))
  -> (Columnar  f x -> Columnar  g x -> m (Columnar  h x))
applyColumnar' _ f fx gx = getColumnar' <$> f (Columnar' fx) (Columnar' gx)

{-------------------------------------------------------------------------------
  Beam test
-------------------------------------------------------------------------------}

largeRecord (defaultLazyOptions { generatePatternSynonym = GenPatSynonym }) [d|
    data TableA (f :: Type -> Type) = TableA {
          taFieldI :: PrimaryKey TableA f
        , taFieldB :: Columnar f Bool
        , taFieldM :: TableB f
        }
      deriving (Show, Eq)

    data TableB (f :: Type -> Type) = TableB {
          tbFieldC :: Columnar f Char
        }
      deriving (Show, Eq)
  |]

instance Table TableA where
  data PrimaryKey TableA f = PrimA (Columnar f Int)

deriving instance Show (Columnar f Int) => Show (PrimaryKey TableA f)
deriving instance Eq   (Columnar f Int) => Eq   (PrimaryKey TableA f)

instance Beamable (PrimaryKey TableA) where
  -- The GHC.Generics instance would normally be fine for primary keys
  zipBeamFieldsM f (PrimA x) (PrimA y) = PrimA <$>
      applyColumnar' (Proxy @Int) f x y

instance Beamable TableA where
  zipBeamFieldsM = gzipBeam

instance Beamable TableB where
  zipBeamFieldsM = gzipBeam

gzipBeam :: forall m table f g h.
     ( Applicative m
     , Generic (table f)
     , Generic (table g)
     , Generic (table h)
     , Generic (table Uninterpreted)
     , Constraints (table Uninterpreted) ZipInterpreted
     , HasNormalForm (BeamInterpretation f) (table f) (table Uninterpreted)
     , HasNormalForm (BeamInterpretation g) (table g) (table Uninterpreted)
     , HasNormalForm (BeamInterpretation h) (table h) (table Uninterpreted)
     )
  => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
  -> table f -> table g -> m (table h)
gzipBeam f a b =
    fmap (to . denormalize1 (Proxy @BeamInterpretation)) $
      Rep.czipWithM
        (Proxy @ZipInterpreted)
        (zipInterpreted f)
        (normalize1 (Proxy @BeamInterpretation) (from a))
        (normalize1 (Proxy @BeamInterpretation) (from b))

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Transform" [
      testCase "gjust"    test_gjust
    , testCase "gswap"    test_gswap
    , testCase "gzipBeam" test_gzipBeam
    ]

test_gjust :: Assertion
test_gjust =
    assertEqual ""
      (justA $ A { aI = I    5, aB = I    True, aS = "hi" })
      (        A { aI = Just 5, aB = Just True, aS = "hi" })

test_gswap :: Assertion
test_gswap =
    assertEqual ""
      (swapB $ B { bI = I        5, bB = Identity True, bS = "hi" })
      (        B { bI = Identity 5, bB = I        True, bS = "hi" })

data Pair x = Pair x x
  deriving (Show, Eq)

test_gzipBeam :: Assertion
test_gzipBeam =
    assertEqual ""
      (unI (zipBeamFieldsM pairup tableA tableA))
      tableA'
  where
    pairup :: Columnar' I x -> Columnar' I x -> I (Columnar' Pair x)
    pairup (Columnar' (I x)) (Columnar' (I y)) = I (Columnar' $ Pair x y)

    tableA :: TableA I
    tableA = TableA {
          taFieldI = PrimA (I 5)
        , taFieldB = I True
        , taFieldM = tableB
        }

    tableB :: TableB I
    tableB = TableB {
          tbFieldC = I 'x'
        }

    tableA' :: TableA Pair
    tableA' = TableA {
          taFieldI = PrimA (Pair 5 5)
        , taFieldB = Pair True True
        , taFieldM = tableB'
        }

    tableB' :: TableB Pair
    tableB' = TableB {
          tbFieldC = Pair 'x' 'x'
        }
