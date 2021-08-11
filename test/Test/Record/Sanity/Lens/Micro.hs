{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Record.Sanity.Lens.Micro (tests) where

import Data.Kind
import Data.Maybe (fromJust)
import Data.SOP
import Lens.Micro (Lens', (^.), (&), (%~))
import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic
import Data.Record.Generic.Lens.Micro
import Data.Record.Generic.SOP
import Data.Record.Generic.Transform
import Data.Record.QQ.CodeGen
import Data.Record.TH

import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  Simple example (no type families)
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
      data Simple (f :: Type -> Type) = MkSimple {
            s1 :: f Int
          , s2 :: f Bool
          , s3 :: f Char
          }
        deriving (Show, Eq)
    |]

simpleExample :: Simple I
simpleExample = [lr| MkSimple {
      s1 = I 5
    , s2 = I True
    , s3 = I 'a'
    } |]

simpleExampleLenses :: Simple (RegularRecordLens Simple f)
simpleExampleLenses = lensesForRegularRecord (Proxy @DefaultInterpretation)

[lr| MkSimple {
      s1 = RegularRecordLens xs1
    , s2 = RegularRecordLens xs2
    , s3 = RegularRecordLens xs3
    } |] = simpleExampleLenses

{-------------------------------------------------------------------------------
  Simplified version of beam's 'Columnar' type'
-------------------------------------------------------------------------------}

data Lenses (tbl :: (Type -> Type) -> Type) (f :: Type -> Type) (x :: Type)

data WrapLens a b = WrapLens (Lens' a b)

type family Columnar f x :: Type where
  Columnar I              x = x
  Columnar (Lenses tbl f) x = WrapLens (tbl f) (Columnar f x)
  Columnar f              x = f x

{-------------------------------------------------------------------------------
  Example with type families, but still regular

  See /next/ example for usage of, and motivation for, 'Lenses'.
-------------------------------------------------------------------------------}

data BeamInterpretation (f :: Type -> Type)

type instance Interpreted (BeamInterpretation f) (Uninterpreted x) = Columnar f x

instance StandardInterpretation BeamInterpretation (RegularRecordLens tbl f)
instance StandardInterpretation BeamInterpretation I

largeRecord defaultPureScript [d|
      data Regular (f :: Type -> Type) = MkRegular {
            r1 :: Columnar f Int
          , r2 :: Columnar f Bool
          , r3 :: Columnar f Char
          }
        deriving (Show, Eq)
    |]

regularExample :: Regular I
regularExample = [lr| MkRegular {
      r1 = 5
    , r2 = True
    , r3 = 'a'
    } |]

regularLenses :: Regular (RegularRecordLens Regular I)
regularLenses = lensesForRegularRecord (Proxy @BeamInterpretation)

[lr| MkRegular {
      r1 = RegularRecordLens xr1
    , r2 = RegularRecordLens xr2
    , r3 = RegularRecordLens xr3
    } |] = regularLenses

{-------------------------------------------------------------------------------
  Beam-like example

  The lenses we generate above have @I x@ as their argument, rather than @x@. In
  beam, the lenses have @Columnar f x@ as their target, which is just @x@ in the
  case that @f == I@. If we want to replicate this, we cannot use
  'lensesForRegularRecord', which gives us 'RegularRecordLens', and must instead
  use the lower-level function 'lensesForHKRecord'. This example is still
  simplified from the beam example because we don't support any form of mixins;
  we insist every field is regular, which allows us to avoid introducing a
  separate type class. See the @beam-large-package@ for full beam integration.
-------------------------------------------------------------------------------}

beamLikeLenses :: forall tbl.
     ( Generic (tbl (Lenses tbl I))
     , Generic (tbl Uninterpreted)
     , Generic (tbl I)
     , HasNormalForm (BeamInterpretation (Lenses tbl I)) (tbl (Lenses tbl I)) (tbl Uninterpreted)
     , HasNormalForm (BeamInterpretation I) (tbl I) (tbl Uninterpreted)
     , Constraints (tbl Uninterpreted) (IsRegularField Uninterpreted)
     )
  => tbl (Lenses tbl I)
beamLikeLenses =
    to . denormalize1 (Proxy @BeamInterpretation) $
      Rep.cmap
        (Proxy @(IsRegularField Uninterpreted))
        aux
        (lensesForHKRecord (Proxy @BeamInterpretation))
  where
    aux :: forall x.
         IsRegularField Uninterpreted x
      => HKRecordLens BeamInterpretation I tbl x
      -> Interpret (BeamInterpretation (Lenses tbl I)) x
    aux (HKRecordLens l) =
        case isRegularField (Proxy @(Uninterpreted x)) of
          RegularField -> Interpret $ WrapLens $
              l
            . standardInterpretationLens (Proxy @BeamInterpretation)
            . unI'

    unI' :: Lens' (I x) x
    unI' f (I x) = I <$> f x

regularBeamLikeLenses :: Regular (Lenses Regular I)
regularBeamLikeLenses = beamLikeLenses

[lr| MkRegular {
      r1 = WrapLens br1
    , r2 = WrapLens br2
    , r3 = WrapLens br3
    } |] = regularBeamLikeLenses

{-------------------------------------------------------------------------------
  Irregular example
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
      data Irregular (f :: Type -> Type) = MkIrregular {
            i1 :: f Int
          , i2 :: f Bool
          , i3 :: Char -- No @f@!
          }
        deriving (Show, Eq)
    |]

irregularExample :: Irregular I
irregularExample = [lr| MkIrregular {
      i1 = I 5
    , i2 = I True
    , i3 = 'a'
    } |]

-- We cannot define this now:
--
-- > irregularLenses :: Irregular (RegularRecordLens Irregular I)
-- > irregularLenses = lensesForRegularRecord (Proxy @DefaultInterpretation)
--
-- It will complain that @Char@ is not equal to
--
-- > Interpreted (DefaultInterpretation (RegularRecordLens Irregular I)) Char
--
-- We can use 'repLenses' to nonetheless get lenses for all fields in
-- 'Irregular', and then translate to an NP so that we can pattern match on it
-- in a type-safe way. Of course, the translation to SOP incurs O(N^2)
-- compile-time cost so this is not a proper solution.
--
-- NOTE: There is not much point using 'repLenses'' here; that is primarily
-- useful only if there is some post-processing step (like done by
-- 'lensesForRegularRecord').
irregularLenses :: NP (Field (SimpleRecordLens (Irregular f))) (MetadataOf (Irregular f))
irregularLenses = fromJust $ toSOP rep
  where
    rep :: Rep (SimpleRecordLens (Irregular f)) (Irregular f)
    rep = lensesForSimpleRecord

(    Field (SimpleRecordLens xi1)
  :* Field (SimpleRecordLens xi2)
  :* Field (SimpleRecordLens xi3)
  :* Nil ) = irregularLenses

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Lens.Micro" [
      testCase "simple_get"    test_simple_get
    , testCase "simple_set"    test_simple_set
    , testCase "regular_get"   test_regular_get
    , testCase "regular_set"   test_regular_set
    , testCase "beamlike_get"  test_beamlike_get
    , testCase "beamlike_set"  test_beamlike_set
    , testCase "irregular_get" test_irregular_get
    , testCase "irregular_set" test_irregular_set
    ]

test_simple_get :: Assertion
test_simple_get =
    assertEqual "" (I True)
      (simpleExample ^. xs2)

test_simple_set :: Assertion
test_simple_set =
    assertEqual "" expected $
      simpleExample & xs1 %~ mapII negate & xs3 %~ mapII succ
  where
    expected :: Simple I
    expected = [lr| MkSimple {
          s1 = I (-5)
        , s2 = I True
        , s3 = I 'b'
        } |]

test_regular_get :: Assertion
test_regular_get =
    assertEqual "" (I True)
      (regularExample ^. xr2)

test_regular_set :: Assertion
test_regular_set =
    assertEqual "" expected $
      regularExample & xr1 %~ mapII negate & xr3 %~ mapII succ
  where
    expected :: Regular I
    expected = [lr| MkRegular {
          r1 = (-5)
        , r2 = True
        , r3 = 'b'
        } |]

test_beamlike_get :: Assertion
test_beamlike_get =
    assertEqual "" True
      (regularExample ^. br2)

test_beamlike_set :: Assertion
test_beamlike_set =
    assertEqual "" expected $
      regularExample & br1 %~ negate & br3 %~ succ
  where
    expected :: Regular I
    expected = [lr| MkRegular {
          r1 = (-5)
        , r2 = True
        , r3 = 'b'
        } |]

test_irregular_get :: Assertion
test_irregular_get =
    assertEqual "" (I True)
      (irregularExample ^. xi2)

test_irregular_set :: Assertion
test_irregular_set =
    assertEqual "" expected $
      irregularExample & xi1 %~ mapII negate & xi3 %~ succ
  where
    expected :: Irregular I
    expected = [lr| MkIrregular {
          i1 = I (-5)
        , i2 = I True
        , i3 = 'b'
        } |]
