module Test.Record.Anonymous.Sanity.Named.Record1 (
    Record(..)
  ) where

-- | Non-anonymous record (for comparison with equivalent anonymous record)
data Record = Record { x :: Bool, y :: Char, z :: () }
  deriving (Show, Eq, Ord)

