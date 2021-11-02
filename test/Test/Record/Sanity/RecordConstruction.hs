{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

#if USE_RDP
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
#endif

--{-# OPTIONS_GHC -Wwarn #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.RecordConstruction (tests) where

import GHC.Records.Compat

import Data.Record.TH

import Test.Tasty
import Test.Tasty.HUnit

-- Test that this works if we don't generate field accessors
-- See <https://gitlab.haskell.org/ghc/ghc/-/issues/19312>
--
-- Use lazy fields so that we can test values with missing fields.
--
-- Test both the case where the name of the type and the name of the constructor
-- are the same and where they are different.
largeRecord (defaultPureScript {allFieldsStrict = False}) [d|
    data R a = MkR { x :: Int, y :: [a] } deriving (Eq, Show)
    data S a = S   { x :: Int, y :: [a] } deriving (Eq, Show)
  |]

inOrder :: R Bool
inOrder = [lr| MkR { x = 1234, y = [True] } |]

outOfOrder :: R Bool
outOfOrder = [lr| MkR { y = [True], x = 1234 } |]

-- | Constructor application
--
-- Occassionally we cannot use the quasi-quoter (for instance, in an
-- applicative context).
constructorApp :: R Bool
constructorApp = [lr| MkR |] 1234 [True]

valueOfS :: S Bool
valueOfS = [lr| S { x = 1234, y = [True] } |]

{-------------------------------------------------------------------------------
  Nested records
-------------------------------------------------------------------------------}

data RegularRecord = RR { a :: Int }
  deriving (Show, Eq)

largeRecord defaultPureScript [d|
    data T = T { x :: Int, y :: S Bool, z :: RegularRecord }
  |]

valueOfT :: T
valueOfT = [lr| T { x = 5
                  , y = S { x = 1234, y = [True] }
                  , z = RR { a = 5 }
                  }
              |]

{-------------------------------------------------------------------------------
  Sanity check
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.RecordConstruction" [
      testCase "allEqual" testAllEqual
    ]

testAllEqual :: Assertion
testAllEqual = do
#if USE_RDP
    assertEqual "inOrder/outOfOrder" inOrder.x  outOfOrder.x
    assertEqual "inOrder/withoutQQ"  inOrder.x  constructorApp.x
    assertEqual "R/S"                inOrder.x  valueOfS.x
    assertEqual "T/S"                valueOfT.y valueOfS
    assertEqual "T/R"                valueOfT.z (RR 5)
#else
    assertEqual "inOrder/outOfOrder" (getField @"x" inOrder)  (getField @"x" outOfOrder)
    assertEqual "inOrder/withoutQQ"  (getField @"x" inOrder)  (getField @"x" constructorApp)
    assertEqual "R/S"                (getField @"x" inOrder)  (getField @"x" valueOfS)
    assertEqual "T/S"                (getField @"y" valueOfT) valueOfS
    assertEqual "T/R"                (getField @"z" valueOfT) (RR 5)
#endif
