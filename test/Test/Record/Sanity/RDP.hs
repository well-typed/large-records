{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

#if USE_RDP
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
#endif

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -fdefer-type-errors #-}

module Test.Record.Sanity.RDP (tests) where

#if !USE_RDP

import Test.Tasty

tests :: TestTree
tests = testGroup "Test.Record.Sanity.RDP" []

#else

---------------- Tests below only compiled when RDP is enabled ----------------

import Data.Data
import Data.Functor.Const
import Data.Record.TH

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import qualified Language.Haskell.Exts as HSE
import qualified Language.Haskell.Meta as HSE.Meta
import qualified Language.Haskell.TH   as TH

{-------------------------------------------------------------------------------
  Simple field selection and override
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
     data R1 = MkR1 { r1_x :: Int, r1_y :: Bool }
       deriving (Show, Eq)
  |]

test_simple :: Assertion
test_simple = do
    assertEqual "x"     r1.r1_x $ 1
    assertEqual "y"     r1.r1_y $ True
    assertEqual "r1_x"  r1_x    $ r1{ r1_x = 2     }
    assertEqual "r1_y"  r1_y    $ r1{ r1_y = False }
    assertEqual "r1_xy" r1_xy   $ r1{ r1_x = 2, r1_y = False }
  where
    r1, r1_x, r1_y, r1_xy :: R1
    r1    = [lr| MkR1 { r1_x = 1, r1_y = True  } |]
    r1_x  = [lr| MkR1 { r1_x = 2, r1_y = True  } |]
    r1_y  = [lr| MkR1 { r1_x = 1, r1_y = False } |]
    r1_xy = [lr| MkR1 { r1_x = 2, r1_y = False } |]

{-------------------------------------------------------------------------------
  Field selection and override when there are overloaded fields
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
    data R2 = MkR2 { a :: Int, b :: Bool } deriving (Show, Eq)
    data R3 = MkR3 { a :: Int, b :: Char } deriving (Show, Eq)
  |]

test_overloaded :: Assertion
test_overloaded = do
    assertEqual "r2.a" r2.a $ 1
    assertEqual "r2.b" r2.b $ True
    assertEqual "r3.a" r3.a $ 2
    assertEqual "r3.b" r3.b $ 'a'
    assertEqual "r2'"  r2'  $ r2{ a = 2   }
    assertEqual "r3'"  r3'  $ r3{ b = 'b' }
  where
    r2, r2' :: R2
    r2  = [lr| MkR2 { a = 1, b = True } |]
    r2' = [lr| MkR2 { a = 2, b = True } |]

    r3 :: R3
    r3  = [lr| MkR3 { a = 2, b = 'a' } |]
    r3' = [lr| MkR3 { a = 2, b = 'b' } |]

{-------------------------------------------------------------------------------
  Nested records

  Both with and without LR.
-------------------------------------------------------------------------------}

data R4A = MkR4A { r4a_x :: Int  , r4a_y :: R5A    } deriving (Show, Eq)
data R5A = MkR5A { r5a_x :: Char , r5a_y :: Double } deriving (Show, Eq)

largeRecord defaultPureScript [d|
    data R4B = MkR4B { r4b_x :: Int  , r4b_y :: R5B    } deriving (Show, Eq)
    data R5B = MkR5B { r5b_x :: Char , r5b_y :: Double } deriving (Show, Eq)
  |]

test_nested :: Assertion
test_nested = do
    assertEqual "r4a_x" r4a.r4a_y.r5a_x $ 'a'
    assertEqual "r4b_x" r4b.r4b_y.r5b_x $ 'a'
  where
    r4a :: R4A
    r4a = MkR4A { r4a_x = 1, r4a_y = r5a }

    r5a :: R5A
    r5a = MkR5A { r5a_x = 'a', r5a_y = 1.2 }

    r4b :: R4B
    r4b = [lr| MkR4B { r4b_x = 1, r4b_y = r5b } |]

    r5b :: R5B
    r5b = [lr| MkR5B { r5b_x = 'a', r5b_y = 1.2 } |]

{-------------------------------------------------------------------------------
  Same tests as above, but RDP usage /within/ a quasi-quote
-------------------------------------------------------------------------------}

test_QQ_simple :: Assertion
test_QQ_simple = do
    assertEqual "x"     [lr| r1.r1_x |] $ 1
    assertEqual "y"     [lr| r1.r1_y |] $ True
    assertEqual "r1_x"  [lr| r1_x    |] $ r1{ r1_x = 2     }
    assertEqual "r1_y"  [lr| r1_y    |] $ r1{ r1_y = False }
    assertEqual "r1_xy" [lr| r1_xy   |] $ r1{ r1_x = 2, r1_y = False }
  where
    r1, r1_x, r1_y, r1_xy :: R1
    r1    = [lr| MkR1 { r1_x = 1, r1_y = True  } |]
    r1_x  = [lr| MkR1 { r1_x = 2, r1_y = True  } |]
    r1_y  = [lr| MkR1 { r1_x = 1, r1_y = False } |]
    r1_xy = [lr| MkR1 { r1_x = 2, r1_y = False } |]

test_QQ_nested :: Assertion
test_QQ_nested = do
    assertEqual "r4a_x" [lr| r4a.r4a_y.r5a_x |] $ 'a'
    assertEqual "r4b_x" [lr| r4b.r4b_y.r5b_x |] $ 'a'
  where
    r4a :: R4A
    r4a = MkR4A { r4a_x = 1, r4a_y = r5a }

    r5a :: R5A
    r5a = MkR5A { r5a_x = 'a', r5a_y = 1.2 }

    r4b :: R4B
    r4b = [lr| MkR4B { r4b_x = 1, r4b_y = r5b } |]

    r5b :: R5B
    r5b = [lr| MkR5B { r5b_x = 'a', r5b_y = 1.2 } |]

{-------------------------------------------------------------------------------
  Collect all tests
-------------------------------------------------------------------------------}

collectAll :: (Data a, Data b) => a -> [b]
collectAll = ($ []) . go
  where
    go :: forall a b. (Data a, Data b) => a -> [b] -> [b]
    go = \x ->
        case cast x of
          Just x' -> (x' :)
          Nothing -> getConst $ gfoldl ap (const $ Const id) x
      where
        ap :: Data x => Const ([b] -> [b]) (x -> y) -> x -> Const ([b] -> [b]) y
        ap (Const acc) x = Const $ acc . go x

hasHaskellSrcMetaBug :: Bool
hasHaskellSrcMetaBug =
    expected /= actual
  where
    expected :: [TH.TyLit]
    expected = collectAll (HSE.Meta.toExp parsed)
    actual   = [TH.StrTyLit "a", TH.StrTyLit "b"]

    parsed :: HSE.Exp HSE.SrcSpanInfo
    HSE.ParseOk parsed = HSE.parseExpWithMode mode "foo @'(\"a\", \"b\")"

    mode :: HSE.ParseMode
    mode = HSE.defaultParseMode {
          HSE.extensions = [
              HSE.EnableExtension HSE.TypeApplications
            , HSE.EnableExtension HSE.DataKinds
            ]
        }

tests :: TestTree
tests = testGroup "Test.Record.Sanity.RDP" [
      testGroup "Outside quasi-quote" [
          testCase "simple"     test_simple
        , testCase "overloaded" test_overloaded
        , testCase "nested"     test_nested
        ]
    , testGroup "Inside quasi-quote" [
         testCase "simple"     test_QQ_simple
       , ( if hasHaskellSrcMetaBug
             then expectFailBecause "bug in haskell-src-meta (#25)"
             else id )
       $ testCase "nested"     test_QQ_nested
       ]
    ]

#endif