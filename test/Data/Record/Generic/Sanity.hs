{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Record.Generic.Sanity (tests) where

import Control.Monad.State (State, evalState, state)
import Data.Proxy
import Data.SOP (NP(..), All, Compose)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.SOP     as SOP
import qualified Data.Vector  as V
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic
import Data.Record.Generic.SOP
-- import Data.Record.Generic.TH
import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  TH test
-------------------------------------------------------------------------------}

-- largeRecord (_ [d| data T = MkT Int |])

{-------------------------------------------------------------------------------
  Small test record

  We derive GHC and SOP generics for interop test.
-------------------------------------------------------------------------------}

data T = MkT Int Bool Char
  deriving (Eq, Show, GHC.Generic, SOP.Generic)

instance Generic T where
  type Constraints c T = (c Int, c Bool, c Char)

  from (MkT i b c) = Rep $ V.fromList [
        unsafeCoerce (I i)
      , unsafeCoerce (I b)
      , unsafeCoerce (I c)
      ]

  to (Rep v) = case V.toList v of
                 [I i, I b, I c] -> MkT (unsafeCoerce i)
                                        (unsafeCoerce b)
                                        (unsafeCoerce c)
                 _otherwise      -> error "to: unexpected vector"

  recordSize _ = 3

  dict :: forall c. Constraints c T => Rep (Dict c) T
  dict = Rep $ V.fromList [
        unsafeCoerce $ dictFor (Proxy @Int)
      , unsafeCoerce $ dictFor (Proxy @Bool)
      , unsafeCoerce $ dictFor (Proxy @Char)
      ]
    where
      dictFor :: c x => Proxy x -> Dict c x
      dictFor _ = Dict

exampleT :: T
exampleT = MkT 5 True 'c'

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
    actual = Rep.pure (unsafeCoerce (K 'a'))

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
        assertEqual "expected matches actual" expected actual'
      FromRepTooMany actual' leftover -> do
        assertEqual "expected matches actual" expected actual'
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
    assertEqual "expected matches actual" (npToRep expected) actual

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Data.Record.Generic.Sanity" [
      testCase "from_to_id" test_from_to_id
    , testCase "pure"       test_pure
    , testCase "sequenceA"  test_sequenceA
    , testCase "zipWithM"   test_zipWithM
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f (g x y)
