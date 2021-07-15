module Test.Record.Prop.Show.Regular (
      Example1(..)
    ) where

import Test.QuickCheck

data Example1 = MkExample1 {
      example1Field1 :: Int
    , example1Field2 :: Bool
    }
  deriving (Show)

instance Arbitrary Example1 where
  arbitrary = MkExample1 <$> arbitrary <*> arbitrary
  shrink (MkExample1 f1 f2) = concat [
        [ MkExample1 f1' f2
        | f1' <- shrink f1
        ]
      , [ MkExample1 f1 f2'
        | f2' <- shrink f2
        ]
      ]