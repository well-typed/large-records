-- | Custom setup to support doctest
--
-- Taken directly from <https://hackage.haskell.org/package/cabal-doctest>.
module Main where

import Distribution.Extra.Doctest (defaultMainWithDoctests)

main :: IO ()
main = defaultMainWithDoctests "doctest-typelet"
