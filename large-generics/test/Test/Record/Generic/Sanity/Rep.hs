{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Record.Generic.Sanity.Rep (tests) where

import Control.Monad.State (State, evalState, state)
import Data.SOP (NP(..), All, Compose)

import qualified Data.SOP as SOP

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Record.Generic
import Data.Record.Generic.LowerBound
import Data.Record.Generic.SOP hiding (glowerBound)

import qualified Data.Record.Generic.SOP as SOP
import qualified Data.Record.Generic.Rep as Rep

import Test.Record.Generic.Infra.Examples

{-------------------------------------------------------------------------------
  For testing purposes, we compare against proper heterogeneous lists
-------------------------------------------------------------------------------}

compareTyped ::
     forall f a. (
       Generic a
     , Constraints a (Compose Eq f)
     , Constraints a (Compose Show f)
     , All IsField (MetadataOf a)
     )
  => NP (Field f) (MetadataOf a) -> Rep f a -> Assertion
compareTyped expected actual =
    case toSOP actual of
      Nothing ->
        assertFailure "compareTyped: incorrect number of fields"
      Just actual' ->
        case toDictAll (Proxy @f) (Proxy @a) (Proxy @Show) of
          Dict ->
            case toDictAll (Proxy @f) (Proxy @a) (Proxy @Eq) of
              Dict -> go expected actual'
  where
    go :: ( All (Compose Eq   (Field f)) fields
          , All (Compose Show (Field f)) fields
          )
       => NP (Field f) fields -> NP (Field f) fields -> Assertion
    go = assertEqual "compareTyped"

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

test_pure :: Assertion
test_pure =
    compareTyped expected actual
  where
    expected :: NP (Field (K Char)) (MetadataOf (ParamRecord () Float))
    expected =
           Field (K 'a')
        :* Field (K 'a')
        :* Field (K 'a')
        :* Field (K 'a')
        :* Field (K 'a')
        :* Nil

    actual :: Rep (K Char) (ParamRecord () Float)
    actual = Rep.pure (K 'a')

test_cpure :: Assertion
test_cpure =
    assertEqual "matches hand-constructed" expected actual
  where
    expected, actual :: ParamRecord () Float
    expected = MkParamRecord 0 False '\x0000' () []
    actual   = glowerBound

test_sequenceA :: Assertion
test_sequenceA =
    compareTyped expected actual
  where
    expected :: NP (Field (K Int)) (MetadataOf (ParamRecord () Float))
    expected =
          flip evalState 0
        $ SOP.hsequence'
        $ SOP.hmap distrib
        $ example
      where
        distrib :: Field (State Int :.: K Int) x
                -> (State Int :.: (Field (K Int))) x
        distrib (Field (Comp x)) = Comp (Field <$> x)

    actual :: Rep (K Int) (ParamRecord () Float)
    actual = flip evalState 0 $ Rep.sequenceA $ SOP.fromSOP example

    example :: NP (Field (State Int SOP.:.: K Int)) (MetadataOf (ParamRecord () Float))
    example =
           Field (Comp (K <$> tick))
        :* Field (Comp (K <$> tick))
        :* Field (Comp (K <$> tick))
        :* Field (Comp (K <$> tick))
        :* Field (Comp (K <$> tick))
        :* Nil

    tick :: State Int Int
    tick = state $ \i -> (i, i + 1)

test_zipWithM :: Assertion
test_zipWithM =
    compareTyped expected actual
  where
    expected :: NP (Field (K Int)) (MetadataOf (ParamRecord () Float))
    expected =
          flip evalState 0
        $ SOP.hsequence'
        $ SOP.hliftA2 tick' x y
      where
        tick' :: Field (K Int) field
              -> Field (K Int) field
              -> (State Int :.: Field (K Int)) field
        tick' (Field a) (Field b) = Comp $ Field <$> tick a b

    actual :: Rep (K Int) (ParamRecord () Float)
    actual = flip evalState 0 $
        Rep.zipWithM tick (fromSOP x) (fromSOP y)

    tick :: K Int x -> K Int x -> State Int (K Int x)
    tick (K a) (K b) = state $ \i -> (K (a + b + i), i + 1)

    x, y :: NP (Field (K Int)) (MetadataOf (ParamRecord () Float))
    x = Field (K 10)
     :* Field (K 11)
     :* Field (K 12)
     :* Field (K 13)
     :* Field (K 14)
     :* Nil
    y = Field (K 20)
     :* Field (K 21)
     :* Field (K 22)
     :* Field (K 23)
     :* Field (K 24)
     :* Nil

test_ord :: Word -> Word -> Bool -> Bool -> Property
test_ord w w' b b'
  | w == w' && b == b' = t1 === t2
  | w == w'            = compare t1 t2 === compare b b'
  | otherwise          = compare t1 t2 === compare w w'
  where
    t1, t2 :: ParamRecord () Float
    t1 = MkParamRecord w  b  'c' () [3.14]
    t2 = MkParamRecord w' b' 'c' () [3.14]

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Generic.Sanity.Rep" [
      testCase     "pure"       test_pure
    , testCase     "cpure"      test_cpure
    , testCase     "sequenceA"  test_sequenceA
    , testCase     "zipWithM"   test_zipWithM
    , testProperty "ord"        test_ord
    ]
