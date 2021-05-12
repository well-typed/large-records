{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Test.Record.Prop.ToFromJSON (tests) where

import Data.Aeson
import Data.Aeson.Types

import Data.Record.TH
import Data.Record.Generic.JSON

import Test.Tasty
import Test.Tasty.QuickCheck

{-------------------------------------------------------------------------------
  Test that gtoJSON and gfromJSON are inverse
-------------------------------------------------------------------------------}

largeRecord (defaultLazyOptions { generatePatternSynonym = True }) [d|
    data A = MkA {
          ax :: Int
        , ay :: Bool
        }
      deriving (Show, Eq)
  |]

instance Arbitrary A where
  arbitrary = MkA <$> arbitrary <*> arbitrary
  shrink MkA{ax = x, ay = y} = concat [
        (\x' -> MkA x' y ) <$> shrink x
      , (\y' -> MkA x  y') <$> shrink y
      ]

instance ToJSON   A where toJSON    = gtoJSON
instance FromJSON A where parseJSON = gparseJSON

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Prop.ToFromJSON" [
      testProperty "tofromJSON" prop_tofromJSON
    ]

prop_tofromJSON :: A -> Property
prop_tofromJSON a =
      counterexample (show (toJSON a))
    $ Right a === parseEither parseJSON (toJSON a)
