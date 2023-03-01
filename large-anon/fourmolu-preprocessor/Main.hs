module Main (main) where

import Ormolu
import System.Environment

import qualified Data.Text.IO as Text

main :: IO ()
main = do
    _nameOrig:inputPath:outputPath:[] <- getArgs
    rendered <- ormoluFile config inputPath
    Text.writeFile outputPath rendered
  where
    config :: Config RegionIndices
    config = defaultConfig
