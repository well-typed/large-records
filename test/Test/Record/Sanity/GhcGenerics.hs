{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuasiQuotes           #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.GhcGenerics (tests) where

import Data.Function (on)
import Data.Record.TH
import Data.Record.TH.Runtime
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Record.Generic as LR
import qualified Data.Record.Generic.Eq as LR
import qualified Generics.Deriving.Eq as GHC
import qualified GHC.Generics as GHC

{-------------------------------------------------------------------------------
  Example large record
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
      data ExampleRecord = MkExampleRecord {
            exampleField1 :: Int
          , exampleField2 :: Bool
          }
    |]

example :: ExampleRecord
example = [lr| MkExampleRecord |] 1 True

{-------------------------------------------------------------------------------
  Show that we can use geqdefault on a large record
-------------------------------------------------------------------------------}

instance ( LR.Generic a
         , LR.Constraints a Eq
         ) => GHC.GEq' (ThroughLRGenerics a) where
  geq' = LR.geq `on` unwrapThroughLRGenerics

allEqualTo :: (GHC.Generic a, GHC.GEq' (GHC.Rep a)) => a -> [a] -> Bool
allEqualTo x = all (GHC.geqdefault x)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.GhcGenerics" [
       testCase "allEqualTo" test_allEqualTo
    ]

test_allEqualTo :: Assertion
test_allEqualTo = assertEqual "" (allEqualTo example [example]) True