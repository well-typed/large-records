module Test.HigherKinded (tests) where

import Data.Functor.Identity
import Data.Record.Generic.LowerBound

import Test.Tasty
import Test.Tasty.HUnit

import qualified HigherKinded.Sized.R010 as AfterHK10
import qualified HigherKinded.Sized.R020 as AfterHK20
import qualified HigherKinded.Sized.R030 as AfterHK30
import qualified HigherKinded.Sized.R040 as AfterHK40
import qualified HigherKinded.Sized.R050 as AfterHK50
import qualified HigherKinded.Sized.R060 as AfterHK60
import qualified HigherKinded.Sized.R070 as AfterHK70
import qualified HigherKinded.Sized.R080 as AfterHK80
import qualified HigherKinded.Sized.R090 as AfterHK90
import qualified HigherKinded.Sized.R100 as AfterH100

tests :: TestTree
tests = testGroup "Test.HigherKinded" [
      testCase "show" test_show
    ]

-- | Check that we can 'Show' the higher-kinded records
test_show :: Assertion
test_show = do
    assertBool "some output" (not . null $ show lr010)
    assertBool "some output" (not . null $ show lr020)
    assertBool "some output" (not . null $ show lr030)
    assertBool "some output" (not . null $ show lr040)
    assertBool "some output" (not . null $ show lr050)
    assertBool "some output" (not . null $ show lr060)
    assertBool "some output" (not . null $ show lr070)
    assertBool "some output" (not . null $ show lr080)
    assertBool "some output" (not . null $ show lr090)
    assertBool "some output" (not . null $ show lr100)
  where
    lr010 = glowerBound :: AfterHK10.R Identity
    lr020 = glowerBound :: AfterHK20.R Identity
    lr030 = glowerBound :: AfterHK30.R Identity
    lr040 = glowerBound :: AfterHK40.R Identity
    lr050 = glowerBound :: AfterHK50.R Identity
    lr060 = glowerBound :: AfterHK60.R Identity
    lr070 = glowerBound :: AfterHK70.R Identity
    lr080 = glowerBound :: AfterHK80.R Identity
    lr090 = glowerBound :: AfterHK90.R Identity
    lr100 = glowerBound :: AfterH100.R Identity

