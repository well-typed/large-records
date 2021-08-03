-- | Miscellaneous utility functions
module Data.Record.TH.CodeGen.Util (
    -- * Monadic combinators
    concatM
  , concatMapM
  ) where

{-------------------------------------------------------------------------------
  Monadic combinators
-------------------------------------------------------------------------------}

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA

concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = concatM . map f

