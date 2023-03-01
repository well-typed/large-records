{-# LANGUAGE Arrows                #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NPlusKPatterns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

#ifdef REBINDABLE_SYNTAX
{-# LANGUAGE RebindableSyntax #-}
#endif

module MODULE_NAME (tests) where

import Control.Applicative (Alternative)
import Control.Arrow (Arrow, ArrowApply, returnA)
import Control.Arrow.Operations (ArrowCircuit(delay))
import Control.Arrow.Transformer.Stream (StreamArrow (..))
import Control.Monad (guard)
import Data.Proxy
import Data.Text (Text)
import GHC.Exts (IsList(..))
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Stream as Stream

#ifdef REBINDABLE_SYNTAX
import Data.Record.Anon.Overloading
#endif

{-------------------------------------------------------------------------------
  Make sure RebindableSyntax causes no issues

  Mostly this is a matter of making sure the right definitions are in scope.
  We go through the special cases mentioned in
  <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html>
  one by one.

  This code is compiled in the context of two modules:

  - "Test.Sanity.RebindableSyntax.Disabled" and
  - "Test.Sanity.RebindableSyntax.Enabled"

  in which rebindable syntax is disabled and enabled, respectively. The
  @.Enabled@ test verifies that we export everything we need to export from
  "Data.Record.Anon.Overloading", and the @.Disabled@ test is there to help
  ensure that we are not exporting anything that we do not need to export.
-------------------------------------------------------------------------------}

test_rebindableSyntax :: Assertion
test_rebindableSyntax = do
    -- An integer literal 368 means “fromInteger (368::Integer)”, rather than
    -- “Prelude.fromInteger (368::Integer)”.
    assertEqual "integerLiteral" 368  $ (368   :: Word)

    -- Fractional literals are handled in just the same way, except that the
    -- translation is fromRational (3.68::Rational).
    assertEqual "fractionalLiteral" 3.68 $ (3.68  :: Float)

    -- String literals are also handled the same way, except that the
    -- translation is fromString ("368"::String).
    assertEqual "stringLiteral" "368" $ ("368" :: Text)

    -- The equality test in an overloaded numeric pattern uses whatever (==) is
    -- in scope.
    let numericPattern :: Word -> Bool
        numericPattern 0 = True
        numericPattern _ = False
    assertEqual "numericPattern" False $ numericPattern 1

    -- The subtraction operation, and the greater-than-or-equal test, in n+k
    -- patterns use whatever (-) and (>=) are in scope.
    let nPlusKPattern :: Word -> Word
        nPlusKPattern 0       = 0
        nPlusKPattern (n + 1) = n
        nPlusKPattern _       = error "impossible"
    assertEqual "nPlusKPattern" 5 $ nPlusKPattern 6

    -- Negation (e.g. “- (f x)”) means “negate (f x)”, both in numeric patterns,
    -- and expressions.
    let negationInPattern :: Int -> Int
        negationInPattern (-1) = 0
        negationInPattern n    = n
    let negationInExpression :: Int -> Int
        negationInExpression = succ
    assertEqual "negationInPattern"    0    $ negationInPattern (-1)
    assertEqual "negationInExpression" (-6) $ - (negationInExpression 5)

    -- Conditionals (e.g. “if e1 then e2 else e3”) means “ifThenElse e1 e2 e3”.
    -- However case expressions are unaffected.
    assertEqual "ifThenElse" 'a' $ if True then 'a' else 'b'

    -- “Do” notation is translated using whatever functions (>>=), (>>), and
    -- fail, are in scope (not the Prelude versions). List comprehensions, mdo
    -- (The recursive do-notation), and parallel array comprehensions, are
    -- unaffected.
    let doNotation :: forall m. (MonadFail m, Alternative m) => m (Maybe Int) -> m Int
        doNotation mInt = do
             Just i <- mInt
             guard (i > 0)
             return i
    assertEqual "doNotation1" (Just 5) $ doNotation (Just (Just 5))
    assertEqual "doNotation2" Nothing  $ doNotation (Just Nothing)
    assertEqual "doNotation3" Nothing  $ doNotation (Nothing)

    -- Arrow notation (see Arrow notation) uses whatever arr, (>>>), first, app,
    -- (|||) and loop functions are in scope. But unlike the other constructs,
    -- the types of these functions must match the Prelude types very closely.
    -- Details are in flux; if you want to use this, ask!
    --
    -- These examples are from
    -- <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/arrows.html#arrow-notation>
    let arr1 ::
             Arrow arr
          => arr Int a -> arr Int a
        arr1 f = proc x -> f -< x+1

        arr2 ::
             ArrowApply arr
          => (Int -> arr Int a) -> arr Int a
        arr2 f = proc x -> f x -<< x+1

        arr3 ::
             Arrow arr
          => arr Int Int -> arr Int Int -> arr Int Int
        arr3 f h = proc x -> do
            y <- f -< x+1
            let z = x+y
            t <- h -< x*z
            returnA -< t+z

        -- Disabled for now
        -- https://gitlab.haskell.org/ghc/ghc/-/issues/23061
        -- arr4 ::
        --      ArrowChoice arr
        --   => (Int -> Int -> Bool) -> arr Int t -> arr (Int, Int) t
        -- arr4 p f = proc (x,y) ->
        --     if p x y
        --       then f -< x+1
        --       else f -< y+2

        counter :: ArrowCircuit a => a Bool Int
        counter = proc reset -> do
            rec output <- returnA -< if reset then 0 else next
                next   <- delay 0 -< output+1
            returnA -< output

    assertEqual "arr1" 7   $ arr1 id  6
    assertEqual "arr2" 13  $ arr2 (+) 6
    assertEqual "arr3" 380 $ arr3 (* 2) (* 3) 6
    assertEqual "counter" [0,1,2,3, 0,1,2,3,4, 0,1,2,3,4, 0,1,2,3,4, 0] $
        case counter of
          StreamArrow f ->
            Stream.take 20 (f $ Stream.cycle [False, False, False, False, True])

    -- List notation, such as [x,y] or [m..n] can also be treated via rebindable
    -- syntax if you use -XOverloadedLists.
    --
    -- <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_lists.html#overloaded-lists>
    let list1, list2, list3, list4, list5, list6, list7 :: CustomList Int
        list1 = []          -- Empty list
        list2 = [1]         -- 1 : []
        list3 = [1,2,10]    -- 1 : 2 : 10 : []
        list4 = [1 .. ]     -- enumFrom 1
        list5 = [1,2 ..]    -- enumFromThen 1 2
        list6 = [1 .. 2]    -- enumFromTo 1 2
        list7 = [1,2 .. 10] -- enumFromThenTo 1 2 10
    assertEqual "list1" (           WrapCL []         ) $ list1
    assertEqual "list2" (           WrapCL [1]        ) $ list2
    assertEqual "list3" (           WrapCL [1,2,10]   ) $ list3
    assertEqual "list4" (takeCL 5 $ WrapCL [1 .. ])     $ takeCL 5 list4
    assertEqual "list5" (takeCL 5 $ WrapCL [1,2 ..]   ) $ takeCL 5 list5
    assertEqual "list6" (           WrapCL [1 .. 2]   ) $ list6
    assertEqual "list7" (           WrapCL [1,2 .. 10]) $ list7

    -- An overloaded label “#foo” means “fromLabel @"foo"”, rather than
    -- “GHC.OverloadedLabels.fromLabel @"foo"”.
    --
    -- <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_labels.html#overloaded-labels>
    let overloadedLabel :: Label
        overloadedLabel = #hi
    assertEqual "overloadedLabel" (Label "hi") $ overloadedLabel

tests :: TestTree
tests = testGroup TESTGROUP_NAME [
      testCase "rebindableSyntax" test_rebindableSyntax
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

newtype CustomList a = WrapCL { unwrapCL :: [a] }
  deriving (Show, Eq)

instance IsList (CustomList a) where
  type Item (CustomList a) = a

  fromList = WrapCL
  toList   = unwrapCL

takeCL :: Int -> CustomList a -> CustomList a
takeCL n (WrapCL xs) = WrapCL (take n xs)

newtype Label = Label String
  deriving (Show, Eq)

instance KnownSymbol x => IsLabel x Label where
  fromLabel = Label $ symbolVal (Proxy @x)