{-# OPTIONS -Wno-orphans #-}

module Test.Prop.Record.Model.Orphans () where

import Data.SOP.BasicFunctors

import Test.QuickCheck

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary    = K <$> arbitrary
  shrink (K a) = K <$> shrink a

instance Arbitrary a => Arbitrary (I a) where
  arbitrary    = I <$> arbitrary
  shrink (I a) = I <$> shrink a