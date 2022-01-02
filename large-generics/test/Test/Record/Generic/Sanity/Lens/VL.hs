{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Test.Record.Generic.Sanity.Lens.VL (tests) where

import Data.Char (toUpper)
import Data.Functor.Identity
import Data.Maybe (fromJust)
import Data.Proxy
import Data.SOP
import Lens.Micro (Lens', (^.), (&), (%~))

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic
import Data.Record.Generic.Lens.VL
import Data.Record.Generic.SOP
import Data.Record.Generic.Transform

import qualified Data.Record.Generic.Rep as Rep

import Test.Record.Generic.Infra.Examples
import Test.Record.Generic.Infra.Beam.Interpretation
import Test.Record.Generic.Infra.Beam.Mini

{-------------------------------------------------------------------------------
  Simple example (no type families)
-------------------------------------------------------------------------------}

regularLenses :: Regular (RegularRecordLens Regular f)
regularLenses = lensesForRegularRecord (Proxy @DefaultInterpretation)

MkRegular {
      regularField1 = RegularRecordLens regularLens1
    , regularField2 = RegularRecordLens regularLens2
    , regularField3 = RegularRecordLens regularLens3
    } = regularLenses

{-------------------------------------------------------------------------------
  Example with type families, but still regular
-------------------------------------------------------------------------------}

mixinTableLenses :: MixinTable (RegularRecordLens MixinTable Identity)
mixinTableLenses = lensesForRegularRecord (Proxy @BeamInterpretation)

MkMixinTable {
      mixinTableField1 = RegularRecordLens mixinTableLens1
    , mixinTableField2 = RegularRecordLens mixinTableLens2
    } = mixinTableLenses

{-------------------------------------------------------------------------------
  Irregular example
-------------------------------------------------------------------------------}

-- We cannot define this now:
--
-- > irregularLenses :: Irregular (RegularRecordLens Irregular I)
-- > irregularLenses = lensesForRegularRecord (Proxy @DefaultInterpretation)
--
-- It will complain that @String@ is not equal to
--
-- > Interpreted (DefaultInterpretation (RegularRecordLens Irregular I)) String
--
-- We can use 'repLenses' to nonetheless get lenses for all fields in
-- 'Irregular', and then translate to an NP so that we can pattern match on it
-- in a type-safe way. Of course, the translation to SOP incurs O(N^2)
-- compile-time cost so this is not a proper solution.
--
-- NOTE: There is not much point using 'repLenses'' here; that is primarily
-- useful only if there is some post-processing step (like done by
-- 'lensesForRegularRecord').
irregularLenses :: NP (Field (SimpleRecordLens (Irregular f)))
                      (MetadataOf (Irregular f))
irregularLenses = fromJust $ toSOP rep
  where
    rep :: Rep (SimpleRecordLens (Irregular f)) (Irregular f)
    rep = lensesForSimpleRecord

-- Unlike the beam tutorial, we match to get these lenses in two steps: first,
-- we get 'SimpleRecordLens' out, which does not rely on impredicativity;
-- then we get the Van Laarhoven lenses out in three separate bindings. This
-- avoids problems with ghc type inference which gets very confused by that
-- pattern match.

irregularLens1' :: SimpleRecordLens (Irregular f) (f Int)
irregularLens2' :: SimpleRecordLens (Irregular f) (f Bool)
irregularLens3' :: SimpleRecordLens (Irregular f) String

(    Field irregularLens1'
  :* Field irregularLens2'
  :* Field irregularLens3'
  :* Nil ) = irregularLenses

irregularLens1 :: Lens' (Irregular f) (f Int)
irregularLens2 :: Lens' (Irregular f) (f Bool)
irregularLens3 :: Lens' (Irregular f) String

SimpleRecordLens irregularLens1 = irregularLens1'
SimpleRecordLens irregularLens2 = irregularLens2'
SimpleRecordLens irregularLens3 = irregularLens3'

{-------------------------------------------------------------------------------
  Beam-like example (using the 'Lenses' indirection).

  This still does not support all beam features; in particular, this only works
  for the regular 'MixinTable', not for the complete 'FullTable'. To do that, we
  would need to introduce a separate type class (instead of 'IsRegularField')
  that then needs to be available for every field, so that we can distinguish
  between mixins and normal ('Columnar') fields. For a completely worked out
  exmaple, see the @beam-large-records@ package
  <https://github.com/well-typed/beam-large-records>.
-------------------------------------------------------------------------------}

beamLikeLenses :: forall tbl.
     ( Generic (tbl (Lenses tbl Identity))
     , Generic (tbl Uninterpreted)
     , Generic (tbl Identity)
     , HasNormalForm (BeamInterpretation (Lenses tbl Identity)) (tbl (Lenses tbl Identity)) (tbl Uninterpreted)
     , HasNormalForm (BeamInterpretation Identity) (tbl Identity) (tbl Uninterpreted)
     , Constraints (tbl Uninterpreted) (IsRegularField Uninterpreted)
     )
  => tbl (Lenses tbl Identity)
beamLikeLenses =
    to . denormalize1 (Proxy @BeamInterpretation) $
      Rep.cmap
        (Proxy @(IsRegularField Uninterpreted))
        aux
        (lensesForHKRecord (Proxy @BeamInterpretation))
  where
    aux :: forall x.
         IsRegularField Uninterpreted x
      => HKRecordLens BeamInterpretation Identity tbl x
      -> Interpret (BeamInterpretation (Lenses tbl Identity)) x
    aux (HKRecordLens l) =
        case isRegularField (Proxy @(Uninterpreted x)) of
          RegularField -> Interpret $ WrapLens $
              l
            . standardInterpretationLens (Proxy @BeamInterpretation)
            . unI'

    unI' :: Lens' (Identity x) x
    unI' f (Identity x) = Identity <$> f x

mixinBeamLikeLenses :: MixinTable (Lenses MixinTable Identity)
mixinBeamLikeLenses = beamLikeLenses

MkMixinTable {
      mixinTableField1 = WrapLens mixinBeamLikeLens1
    , mixinTableField2 = WrapLens mixinBeamLikeLens2
    } = mixinBeamLikeLenses

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Generic.Sanity.Lens.VL" [
      testCase "regular_get"   test_regular_get
    , testCase "regular_set"   test_regular_set
    , testCase "mixin_get"     test_mixin_get
    , testCase "mixin_set"     test_mixin_set
    , testCase "irregular_get" test_irregular_get
    , testCase "irregular_set" test_irregular_set
    , testCase "beamlike_get"  test_beamlike_get
    , testCase "beamlike_set"  test_beamlike_set
    ]

test_regular_get :: Assertion
test_regular_get =
    assertEqual "" (I True)
      (exampleRegular ^. regularLens2)

test_regular_set :: Assertion
test_regular_set =
    assertEqual "" expected $
        exampleRegular
      & regularLens1 %~ mapII negate
      & regularLens3 %~ mapII (map toUpper)
  where
    expected :: Regular I
    expected = MkRegular {
          regularField1 = I (-5)
        , regularField2 = I True
        , regularField3 = I "A"
        }

test_mixin_get :: Assertion
test_mixin_get =
    assertEqual "" 3.14
      (exampleMixinTable ^. mixinTableLens2)

test_mixin_set :: Assertion
test_mixin_set =
    assertEqual "" expected $
        exampleMixinTable
      & mixinTableLens1 %~ succ
      & mixinTableLens2 %~ negate
  where
    expected :: MixinTable Identity
    expected = MkMixinTable {
          mixinTableField1 = 'b'
        , mixinTableField2 = -3.14
        }

test_irregular_get :: Assertion
test_irregular_get =
    assertEqual "" (I True)
      (exampleIrregular ^. irregularLens2)

test_irregular_set :: Assertion
test_irregular_set =
    assertEqual "" expected $
        exampleIrregular
      & irregularLens1 %~ mapII negate
      & irregularLens3 %~ map toUpper
  where
    expected :: Irregular I
    expected = MkIrregular {
          irregularField1 = I (-1234)
        , irregularField2 = I True
        , irregularField3 = "HI"
        }

test_beamlike_get :: Assertion
test_beamlike_get =
    assertEqual "" 3.14
      (exampleMixinTable ^. mixinBeamLikeLens2)

test_beamlike_set :: Assertion
test_beamlike_set =
    assertEqual "" expected $
        exampleMixinTable
      & mixinBeamLikeLens1 %~ succ
      & mixinBeamLikeLens2 %~ negate
  where
    expected :: MixinTable Identity
    expected = MkMixinTable {
          mixinTableField1 = 'b'
        , mixinTableField2 = -3.14
        }
