{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Sanity.Named.Record2 (
    Record(..)
  ) where

-- | Non-anonymous record (for comparison with equivalent anonymous record)
data Record f = ANON_F { y :: f Char, x :: f Bool }

deriving instance (Show (f Char), Show (f Bool)) => Show (Record f)
deriving instance (Eq   (f Char), Eq   (f Bool)) => Eq   (Record f)
deriving instance (Ord  (f Char), Ord  (f Bool)) => Ord  (Record f)

