{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE PatternSynonyms         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns            #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Data.Record.Generic.Sanity (tests) where

import Control.Monad.State (State, evalState, state)
import Data.Proxy
import Data.SOP (NP(..), All, Compose)

import qualified Data.SOP                   as SOP
import qualified Generics.SOP               as SOP
import qualified Generics.SOP.Metadata      as SOP
import qualified Generics.SOP.Type.Metadata as SOP.T
import qualified Generics.SOP.Eq            as SOP

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic
import Data.Record.Generic.Show
import Data.Record.Generic.SOP
import Data.Record.Generic.TH
import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  Small test record
-------------------------------------------------------------------------------}

largeRecord (defaultOptions { generatePatternSynonym = True }) [d|
  data T = MkT {
      tInt  :: Int
    , tBool :: Bool
    , tChar :: Char
    }
  |]

exampleT :: T
exampleT = MkT 5 True 'c'

{-------------------------------------------------------------------------------
  Handwritten SOP instance

  This allows us to compare the untyped representation used by @large-records@
  to the strongly typed version from @generics-sop@.
-------------------------------------------------------------------------------}

instance SOP.Generic T where
  type Code T = '[[Int, Bool, Char]]
  from (MkT i b c) = SOP.SOP (SOP.Z (I i :* I b :* I c :* Nil))
  to (SOP.SOP (SOP.Z (I i :* I b :* I c :* Nil))) = MkT i b c
  to (SOP.SOP (SOP.S x)) = case x of {}

instance SOP.HasDatatypeInfo T where
  type DatatypeInfoOf T =
    'SOP.T.ADT
      "Data.Record.Generic.Sanity"
      "T"
      '[ 'SOP.T.Record "MkT" '[
          'SOP.T.FieldInfo "tInt"
        , 'SOP.T.FieldInfo "tBool"
        , 'SOP.T.FieldInfo "tChar"
        ]]
      '[ '[
          'SOP.T.StrictnessInfo
            'SOP.NoSourceUnpackedness
            'SOP.NoSourceStrictness
            'SOP.DecidedLazy
        , 'SOP.T.StrictnessInfo
            'SOP.NoSourceUnpackedness
            'SOP.NoSourceStrictness
            'SOP.DecidedLazy
        , 'SOP.T.StrictnessInfo
            'SOP.NoSourceUnpackedness
            'SOP.NoSourceStrictness
            'SOP.DecidedLazy
        ]]

  datatypeInfo _ = SOP.T.demoteDatatypeInfo (Proxy @(SOP.DatatypeInfoOf T))

{-------------------------------------------------------------------------------
  Temporary: use SOP generics to derive 'Eq' and 'Show'

  TODO: We'll want to use our own generics to do this, and derive it
  automatically. Once we do, remove @basic-sop@ dependency.
-------------------------------------------------------------------------------}

instance Show T where
  showsPrec = gshowsPrec

instance Eq T where (==) = SOP.geq

{-------------------------------------------------------------------------------
  Example type class (for constructing values)
-------------------------------------------------------------------------------}

class Default a where
  defaultValue :: a

instance Default Int  where defaultValue = 0
instance Default Bool where defaultValue = False
instance Default Char where defaultValue = '\x0000'

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

test_from_to_id :: Assertion
test_from_to_id =
    assertEqual "from . to = id" expected actual
  where
    expected, actual :: T
    expected = exampleT
    actual   = (to . from) exampleT

test_pure :: Assertion
test_pure =
    compareTyped expected actual
  where
    expected :: NP (K Char) (Fields T)
    expected = K 'a' :* K 'a' :* K 'a' :* Nil

    actual :: Rep (K Char) T
    actual = Rep.pure (K 'a')

test_cpure :: Assertion
test_cpure =
    assertEqual "matches hand-constructed" expected actual
  where
    expected, actual :: T
    expected = MkT 0 False '\x0000'
    actual   = to $ Rep.cpure (Proxy @Default) (I defaultValue)

test_sequenceA :: Assertion
test_sequenceA =
    compareTyped expected actual
  where
    expected :: NP (K Int) (Fields T)
    expected = flip evalState 0 $ SOP.hsequence' $ example

    actual :: Rep (K Int) T
    actual = flip evalState 0 $ Rep.sequenceA $ npToRep example

    example :: NP (State Int SOP.:.: K Int) (Fields T)
    example =
           Comp (K <$> tick)
        :* Comp (K <$> tick)
        :* Comp (K <$> tick)
        :* Nil

    tick :: State Int Int
    tick = state $ \i -> (i, i + 1)

test_zipWithM :: Assertion
test_zipWithM =
    compareUntyped expected actual
  where
    expected :: NP (K Int) (Fields T)
    expected = flip evalState 0 $
        SOP.hsequence' $ SOP.hliftA2 (Comp .: tick) x y

    actual :: Rep (K Int) T
    actual = flip evalState 0 $
        Rep.zipWithM tick (npToRep x) (npToRep y)

    tick :: K Int x -> K Int x -> State Int (K Int x)
    tick (K a) (K b) = state $ \i -> (K (a + b + i), i + 1)

    x, y :: NP (K Int) (Fields T)
    x = K 0 :* K 1 :* K 2 :* Nil
    y = K 3 :* K 4 :* K 5 :* Nil

{-------------------------------------------------------------------------------
  For testing purposes, we compare against proper heterogeneous lists
-------------------------------------------------------------------------------}

compareTyped ::
     forall f a.
     (All (Compose Eq f) (Fields a), All (Compose Show f) (Fields a))
  => NP f (Fields a) -> Rep f a -> Assertion
compareTyped expected actual =
    case npFromRep (Proxy @(Compose Show f)) actual of
      FromRepExact actual' ->
        assertEqual "matches exactly" expected actual'
      FromRepTooMany actual' leftover -> do
        assertEqual "matches prefix" expected actual'
        assertFailure $ concat [
            show (length leftover)
          , " fields left over"
          ]
      FromRepTooFew actual' ->
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
    assertEqual "untyped representation matches" (npToRep expected) actual

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Data.Record.Generic.Sanity" [
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
