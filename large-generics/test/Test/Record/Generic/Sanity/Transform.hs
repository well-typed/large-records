{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Record.Generic.Sanity.Transform (tests) where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.SOP.BasicFunctors
import GHC.TypeLits (Nat)

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic
import Data.Record.Generic.Transform

import qualified Data.Record.Generic.Rep as Rep
import qualified Generics.SOP            as SOP

import Test.Record.Generic.Infra.Beam.Interpretation
import Test.Record.Generic.Infra.Beam.Mini
import Test.Record.Generic.Infra.Examples

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
     ( SOP.IsProductType (x I)     fields
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

justIrregular :: Irregular I -> Irregular Maybe
justIrregular = gjust

{-------------------------------------------------------------------------------
  Example with two variables
-------------------------------------------------------------------------------}

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

swapMultiFun :: MultiFun I Identity -> MultiFun Identity I
swapMultiFun = gswap

{-------------------------------------------------------------------------------
  Beam test
-------------------------------------------------------------------------------}

instance Beamable (PrimaryKey FullTable) where
  -- The GHC.Generics instance would normally be fine for primary keys
  zipBeamFieldsM f (PrimA x) (PrimA y) = PrimA <$>
      applyColumnar' (Proxy @Int) f x y

instance Beamable FullTable  where zipBeamFieldsM = gzipBeam
instance Beamable MixinTable where zipBeamFieldsM = gzipBeam

instance Table FullTable

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Generic.Sanity.Transform" [
      testCase "gjust"    test_gjust
    , testCase "gswap"    test_gswap
    , testCase "gzipBeam" test_gzipBeam
    ]

test_gjust :: Assertion
test_gjust =
    assertEqual ""
      (justIrregular $ MkIrregular (I    5) (I    True) "hi")
      (                MkIrregular (Just 5) (Just True) "hi")

test_gswap :: Assertion
test_gswap =
    assertEqual ""
      (swapMultiFun $ MkMultiFun (I        5) (Identity True) "hi")
      (               MkMultiFun (Identity 5) (I        True) "hi")

data Pair x = Pair x x
  deriving (Show, Eq)

test_gzipBeam :: Assertion
test_gzipBeam =
    assertEqual ""
      (unI (zipBeamFieldsM pairup parentTable parentTable))
      parentTable'
  where
    pairup :: Columnar' I x -> Columnar' I x -> I (Columnar' Pair x)
    pairup (Columnar' (I x)) (Columnar' (I y)) = I (Columnar' $ Pair x y)

    parentTable :: FullTable I
    parentTable = MkFullTable {
          fullTableField1 = PrimA (I 5)
        , fullTableField2 = I True
        , fullTableField3 = mixinTable
        }

    mixinTable :: MixinTable I
    mixinTable = MkMixinTable {
          mixinTableField1 = I 'x'
        , mixinTableField2 = I 3.14
        }

    parentTable' :: FullTable Pair
    parentTable' = MkFullTable {
          fullTableField1 = PrimA (Pair 5 5)
        , fullTableField2 = Pair True True
        , fullTableField3 = mixinTable'
        }

    mixinTable' :: MixinTable Pair
    mixinTable' = MkMixinTable {
          mixinTableField1 = Pair 'x' 'x'
        , mixinTableField2 = Pair 3.14 3.14
        }
