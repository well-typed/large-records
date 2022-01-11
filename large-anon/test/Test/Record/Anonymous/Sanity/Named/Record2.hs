module Test.Record.Anonymous.Sanity.Named.Record2 (
    Record(..)
  ) where

-- | Non-anonymous record (for comparison with equivalent anonymous record)
data Record = Record { y :: Char, x :: Bool }
  deriving (Show, Eq, Ord)

