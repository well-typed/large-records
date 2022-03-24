-- | Doctest driver
--
-- This entire module is taken directly from
-- <https://hackage.haskell.org/package/cabal-doctest>.
module Main where

import Build_doctests (Component (..), components)
import Data.Foldable (for_)
import System.Environment (unsetEnv)
import Test.DocTest (doctest)

main :: IO ()
main = do
    for_ components $ \(Component name flags pkgs sources) -> do
      print name
      putStrLn "----------------------------------------"
      let args = flags ++ pkgs ++ sources
      unsetEnv "GHC_ENVIRONMENT"
      doctest args
