-- | Miscellaneous utility functions
module Data.Record.TH.CodeGen.Util (
    -- * Monadic combinators
    concatM
  , concatMapM
    -- * Strings
  , firstToLower
  ) where

import Data.Char (toLower) 

{-------------------------------------------------------------------------------
  Monadic combinators
-------------------------------------------------------------------------------}

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA

concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = concatM . map f

{-------------------------------------------------------------------------------
  Strings
-------------------------------------------------------------------------------}

firstToLower :: String -> String
firstToLower []     = []
firstToLower (c:cs) = toLower c : cs
