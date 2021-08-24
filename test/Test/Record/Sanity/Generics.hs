{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

-- | Sanity check that the generic functions are not totally broken
--
-- These are not proper tests, merely intended to catch egregious refactoring
-- mistakes.
module Test.Record.Sanity.Generics (tests) where

import Control.Monad.State (State, evalState, state)
import Data.Proxy
import Data.SOP (NP(..), All, Compose)

import qualified Data.SOP                   as SOP
import qualified Generics.SOP               as SOP
import qualified Generics.SOP.Metadata      as SOP
import qualified Generics.SOP.Type.Metadata as SOP.T

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Record.Generic
import Data.Record.Generic.LowerBound
import Data.Record.Generic.SOP hiding (glowerBound)
import Data.Record.TH

import qualified Data.Record.Generic.SOP as SOP
import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  Small test record
-------------------------------------------------------------------------------}

largeRecord (defaultLazyOptions { generatePatternSynonym = GenPatSynonym }) [d|
  data T a b = MkT {
        tInt   :: Word
      , tBool  :: Bool
      , tChar  :: Char
      , tA     :: a
      , tListB :: [b]
      }
    deriving (Eq, Ord, Show)
  |]

exampleT :: T () Float
exampleT = MkT 5 True 'c' () [3.14]

_silenceWarnings :: ()
_silenceWarnings = const () $ (
      tInt
    , tBool
    , tChar
    , tA
    , tListB
    )

{-------------------------------------------------------------------------------
  Handwritten SOP instance

  This allows us to compare the untyped representation used by @large-records@
  to the strongly typed version from @generics-sop@.
-------------------------------------------------------------------------------}

instance SOP.Generic (T a b) where
  type Code (T a b) = '[[Word, Bool, Char, a, [b]]]
  from (MkT i b c a bs) = SOP.SOP (SOP.Z (I i :* I b :* I c :* I a :* I bs :* Nil))
  to (SOP.SOP (SOP.Z (I i :* I b :* I c :* I a :* I bs :* Nil))) = MkT i b c a bs
  to (SOP.SOP (SOP.S x)) = case x of {}

type DefaultBang =
       'SOP.T.StrictnessInfo
          'SOP.NoSourceUnpackedness
          'SOP.NoSourceStrictness
          'SOP.DecidedLazy

instance SOP.HasDatatypeInfo (T a b) where
  type DatatypeInfoOf (T a b) =
    'SOP.T.ADT
      "Data.Record.Generic.Sanity"
      "T"
      '[ 'SOP.T.Record "MkT" '[
          'SOP.T.FieldInfo "tInt"
        , 'SOP.T.FieldInfo "tBool"
        , 'SOP.T.FieldInfo "tChar"
        , 'SOP.T.FieldInfo "tA"
        , 'SOP.T.FieldInfo "tListB"
        ]]
      '[ '[
          DefaultBang
        , DefaultBang
        , DefaultBang
        , DefaultBang
        , DefaultBang
        ]]

  datatypeInfo _ =
      SOP.T.demoteDatatypeInfo $ Proxy @(SOP.DatatypeInfoOf (T a b))

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

test_from_to_id :: Assertion
test_from_to_id =
    assertEqual "from . to = id" expected actual
  where
    expected, actual :: T () Float
    expected = exampleT
    actual   = (to . from) exampleT

test_pure :: Assertion
test_pure =
    compareTyped expected actual
  where
    expected :: NP (Field (K Char)) (MetadataOf (T () Float))
    expected =
           Field (K 'a')
        :* Field (K 'a')
        :* Field (K 'a')
        :* Field (K 'a')
        :* Field (K 'a')
        :* Nil

    actual :: Rep (K Char) (T () Float)
    actual = Rep.pure (K 'a')

test_cpure :: Assertion
test_cpure =
    assertEqual "matches hand-constructed" expected actual
  where
    expected, actual :: T () Float
    expected = MkT 0 False '\x0000' () []
    actual   = glowerBound

test_sequenceA :: Assertion
test_sequenceA =
    compareTyped expected actual
  where
    expected :: NP (Field (K Int)) (MetadataOf (T () Float))
    expected =
          flip evalState 0
        $ SOP.hsequence'
        $ SOP.hmap distrib
        $ example
      where
        distrib :: Field (State Int :.: K Int) x
                -> (State Int :.: (Field (K Int))) x
        distrib (Field (Comp x)) = Comp (Field <$> x)

    actual :: Rep (K Int) (T () Float)
    actual = flip evalState 0 $ Rep.sequenceA $ SOP.fromSOP example

    example :: NP (Field (State Int SOP.:.: K Int)) (MetadataOf (T () Float))
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
    expected :: NP (Field (K Int)) (MetadataOf (T () Float))
    expected =
          flip evalState 0
        $ SOP.hsequence'
        $ SOP.hliftA2 tick' x y
      where
        tick' :: Field (K Int) field
              -> Field (K Int) field
              -> (State Int :.: Field (K Int)) field
        tick' (Field a) (Field b) = Comp $ Field <$> tick a b

    actual :: Rep (K Int) (T () Float)
    actual = flip evalState 0 $
        Rep.zipWithM tick (fromSOP x) (fromSOP y)

    tick :: K Int x -> K Int x -> State Int (K Int x)
    tick (K a) (K b) = state $ \i -> (K (a + b + i), i + 1)

    x, y :: NP (Field (K Int)) (MetadataOf (T () Float))
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
    t1, t2 :: T () Float
    t1 = MkT w  b  'c' () [3.14]
    t2 = MkT w' b' 'c' () [3.14]

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
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity" [
      testCase     "from_to_id" test_from_to_id
    , testCase     "pure"       test_pure
    , testCase     "sequenceA"  test_sequenceA
    , testCase     "zipWithM"   test_zipWithM
    , testCase     "cpure"      test_cpure
    , testProperty "ord"        test_ord
    ]
