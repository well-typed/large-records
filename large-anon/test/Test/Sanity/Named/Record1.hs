{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Sanity.Named.Record1 (
    Record(..)
  ) where

-- | Non-anonymous record (for comparison with equivalent anonymous record)
data Record f = Record { x :: f Bool, y :: f Char, z :: f () }

deriving instance (Show (f Bool), Show (f Char), Show (f ())) => Show (Record f)
deriving instance (Eq   (f Bool), Eq   (f Char), Eq   (f ())) => Eq   (Record f)
deriving instance (Ord  (f Bool), Ord  (f Char), Ord  (f ())) => Ord  (Record f)
