{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}

{-# OPTIONS_GHC -ddump-splices #-}

-- | Sanity check that the generic functions are not totally broken
--
-- These are not proper tests, merely intended to catch egregious refactoring
-- mistakes.
module Test.Record.Generic.Sanity (tests) where

import Control.Monad.State (State, evalState, state)
import Data.Proxy
import Data.SOP (NP(..), All, Compose)

import qualified Data.SOP                   as SOP
import qualified Generics.SOP               as SOP
import qualified Generics.SOP.Metadata      as SOP
import qualified Generics.SOP.Type.Metadata as SOP.T

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic
import Data.Record.Generic.LowerBound
import Data.Record.Generic.SOP (Fields)
import Data.Record.Generic.TH

import qualified Data.Record.Generic.SOP as SOP
import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  Small test record
-------------------------------------------------------------------------------}

largeRecord (defaultLazyOptions { generatePatternSynonym = True }) [d|
  data T a b = MkT {
        tInt   :: Word
      , tBool  :: Bool
      , tChar  :: Char
      , tA     :: a
      , tListB :: [b]
      }
    deriving (Eq, Show)
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
    expected :: NP (K Char) (Fields (T () Float))
    expected = K 'a' :* K 'a' :* K 'a' :* K 'a' :* K 'a' :* Nil

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
    expected :: NP (K Int) (Fields (T () Float))
    expected = flip evalState 0 $ SOP.hsequence' $ example

    actual :: Rep (K Int) (T () Float)
    actual = flip evalState 0 $ Rep.sequenceA $ SOP.npToRep example

    example :: NP (State Int SOP.:.: K Int) (Fields (T () Float))
    example =
           Comp (K <$> tick)
        :* Comp (K <$> tick)
        :* Comp (K <$> tick)
        :* Comp (K <$> tick)
        :* Comp (K <$> tick)
        :* Nil

    tick :: State Int Int
    tick = state $ \i -> (i, i + 1)

test_zipWithM :: Assertion
test_zipWithM =
    compareUntyped expected actual
  where
    expected :: NP (K Int) (Fields (T () Float))
    expected = flip evalState 0 $
        SOP.hsequence' $ SOP.hliftA2 (Comp .: tick) x y

    actual :: Rep (K Int) (T () Float)
    actual = flip evalState 0 $
        Rep.zipWithM tick (SOP.npToRep x) (SOP.npToRep y)

    tick :: K Int x -> K Int x -> State Int (K Int x)
    tick (K a) (K b) = state $ \i -> (K (a + b + i), i + 1)

    x, y :: NP (K Int) (Fields (T () Float))
    x = K 10 :* K 11 :* K 12 :* K 13 :* K 14 :* Nil
    y = K 20 :* K 21 :* K 22 :* K 23 :* K 24 :* Nil

{-------------------------------------------------------------------------------
  For testing purposes, we compare against proper heterogeneous lists
-------------------------------------------------------------------------------}

compareTyped ::
     forall f a.
     (All (Compose Eq f) (Fields a), All (Compose Show f) (Fields a))
  => NP f (Fields a) -> Rep f a -> Assertion
compareTyped expected actual =
    case SOP.npFromRep (Proxy @(Compose Show f)) actual of
      SOP.FromRepExact actual' ->
        assertEqual "matches exactly" expected actual'
      SOP.FromRepTooMany actual' leftover -> do
        assertEqual "matches prefix" expected actual'
        assertFailure $ concat [
            show (length leftover)
          , " fields left over"
          ]
      SOP.FromRepTooFew actual' ->
        assertFailure $ concat [
            "Not enough fields: expected "
          , show expected
          , " but got "
          , show actual'
          ]

-- | Variation on 'compareTyped' that compares based on the /untyped/ version
--
-- This can be more informative, as we can show all values, but is also
-- applicable in degenerate circumstances (e.g. @f == K x@ for some @x@).
compareUntyped ::
     (Show (Rep f a), Eq (Rep f a))
  => NP f (Fields a) -> Rep f a -> Assertion
compareUntyped expected actual =
    assertEqual "untyped representation matches" (SOP.npToRep expected) actual

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Generic.Sanity" [
      testCase "from_to_id" test_from_to_id
    , testCase "pure"       test_pure
    , testCase "sequenceA"  test_sequenceA
    , testCase "zipWithM"   test_zipWithM
    , testCase "cpure"      test_cpure
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f (g x y)
