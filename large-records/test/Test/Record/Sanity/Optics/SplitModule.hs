{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Test that the RDP plugin can use records created by large-records
--
-- For this test, we split the modules: one using the large-records plugin,
-- one using the RDP plugin. See also "Test.Record.Sanity.RDP.SingleModule".
module Test.Record.Sanity.Optics.SplitModule (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Optics.Core

import Test.Record.Sanity.Optics.SplitModule.RecordDef

import Optics.TH (makeFieldLabelsNoPrefix)

{-------------------------------------------------------------------------------
  Simple field selection and override
-------------------------------------------------------------------------------}

test_simple :: Assertion
test_simple = do
    assertEqual "x"     (r1 ^. #r1_x) $ 1
    assertEqual "y"     (r1 ^. #r1_y) $ True
    assertEqual "r1_x"  r1_x'   $ r1 & #r1_x .~ 2
    assertEqual "r1_y"  r1_y'   $ r1 & #r1_y .~ False
    assertEqual "r1_xy" r1_xy'  $ r1 & #r1_x .~ 2 & #r1_y .~ False
  where
    r1, r1_x', r1_y', r1_xy' :: R1
    r1     = MkR1 { r1_x = 1, r1_y = True  }
    r1_x'  = MkR1 { r1_x = 2, r1_y = True  }
    r1_y'  = MkR1 { r1_x = 1, r1_y = False }
    r1_xy' = MkR1 { r1_x = 2, r1_y = False }

{-------------------------------------------------------------------------------
  Field selection and override when there are overloaded fields
-------------------------------------------------------------------------------}

test_overloaded :: Assertion
test_overloaded = do
    assertEqual "r2.a" (r2 ^. #a) $ 1
    assertEqual "r2.b" (r2 ^. #b) $ True
    assertEqual "r3.a" (r3 ^. #a) $ 2
    assertEqual "r3.b" (r3 ^. #b) $ 'a'
    assertEqual "r2'"  r2'  $ r2 & #a .~ 2
    assertEqual "r3'"  r3'  $ r3 & #b .~ 'b'
  where
    r2, r2' :: R2
    r2  = MkR2 { a = 1, b = True }
    r2' = MkR2 { a = 2, b = True }

    r3 :: R3
    r3  = MkR3 { a = 2, b = 'a' }
    r3' = MkR3 { a = 2, b = 'b' }

{-------------------------------------------------------------------------------
  Nested records

  Both with and without LR.
-------------------------------------------------------------------------------}

data R4_WOutLR = MkR4_WOutLR { r4_woutLR_x :: Int, r4_woutLR_y :: R5_WOutLR }
  deriving (Show, Eq)

data R5_WOutLR = MkR5_WOutLR { r5_woutLR_x :: Char, r5_woutLR_y :: Double }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''R4_WOutLR
makeFieldLabelsNoPrefix ''R5_WOutLR

test_nested :: Assertion
test_nested = do
    assertEqual "r4_woutLR_x" (r4_woutLR ^. #r4_woutLR_y % #r5_woutLR_x) $ 'a'
    assertEqual "r4_withLR_x" (r4_WithLR ^. #r4_withLR_y % #r5_withLR_x) $ 'a'
  where
    r4_woutLR :: R4_WOutLR
    r4_woutLR = MkR4_WOutLR { r4_woutLR_x = 1, r4_woutLR_y = r5_woutLR }

    r5_woutLR :: R5_WOutLR
    r5_woutLR = MkR5_WOutLR { r5_woutLR_x = 'a', r5_woutLR_y = 1.2 }

    r4_WithLR :: R4_WithLR
    r4_WithLR = MkR4_WithLR { r4_withLR_x = 1, r4_withLR_y = r5_WithLR }

    r5_WithLR :: R5_WithLR
    r5_WithLR = MkR5_WithLR { r5_withLR_x = 'a', r5_withLR_y = 1.2 }

{-------------------------------------------------------------------------------
  Collect all tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Optics.SplitModule" [
      testCase "simple"     test_simple
    , testCase "overloaded" test_overloaded
    , testCase "nested"     test_nested
    ]
