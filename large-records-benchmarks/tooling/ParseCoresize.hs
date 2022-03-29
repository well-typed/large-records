{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Main (main) where

import Conduit
import Data.List (sort)
import Options.Applicative
import System.IO
import Text.Read (readMaybe)
import Text.Regex.PCRE.Light (Regex)

import qualified Data.ByteString.Lazy.UTF8 as BS.Lazy.UTF8
import qualified Data.Conduit.Combinators  as C
import qualified Data.Conduit.List         as C.List
import qualified Data.Csv                  as Csv
import qualified Data.Vector               as V

import Tooling

{-------------------------------------------------------------------------------
  Command line options
-------------------------------------------------------------------------------}

data Options = Options {
      -- | dist directory (defaults to dist-newstyle)
      optsDistDir :: FilePath

      -- | Pattern match
    , optsMatch :: Regex

      -- | Output filename
    , optsOutput :: Maybe FilePath
    }

parseOptions :: Parser Options
parseOptions = Options
    <$> parseDistDir
    <*> parseMatch
    <*> parseOutput

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (parseOptions <**> helper) $ mconcat [
        fullDesc
      , progDesc "Process -ddump-ds-preopt, -ddump-ds or -ddump-simpl files"
      ]

{-------------------------------------------------------------------------------
  Stats
-------------------------------------------------------------------------------}

data Stats = Stats {
      statsTerms     :: !Int
    , statsTypes     :: !Int
    , statsCoercions :: !Int
    , statsTotal     :: !Int
    }
  -- The 'Ord' instance dictates the default ordering in the CSV file.
  deriving (Show, Eq, Ord)

instance Csv.ToRecord Stats where
  toRecord Stats{..} = V.fromList [
        Csv.toField statsTerms
      , Csv.toField statsTypes
      , Csv.toField statsCoercions
      , Csv.toField statsTotal
      ]

parseStats :: FilePath -> String -> Stats
parseStats fp raw =
    case prep raw of
      "{terms:" : terms  : "types:" : types : "coercions:" : coercions : _ ->
        case (readMaybe terms, readMaybe types, readMaybe coercions) of
          (Nothing, _, _) ->
            error $ "processFile: could not parse terms " ++ show terms
          (_, Nothing, _) ->
            error $ "processFile: could not parse terms " ++ show terms
          (_, _, Nothing) ->
            error $ "processFile: could not parse terms " ++ show terms
          (Just terms', Just types', Just coercions') -> do
            Stats {
                statsTerms     = terms'
              , statsTypes     = types'
              , statsCoercions = coercions'
              , statsTotal     = terms' + types' + coercions'
              }
      _otherwise ->
        error $ "processFile: could not parse " ++ fp
  where
    prep :: String -> [String]
    prep =
        dropWhile (/= "{terms:")
      . words
      . filter (`notElem` ",.")

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

processFile :: FilePath -> IO (Maybe Stats)
processFile fp = withFile fp ReadMode $ \h -> do
    !stats <- parseStats fp <$> hGetContents h
    return $ Just stats

main :: IO ()
main = do
    Options{..} <- getOptions
    stats <- runResourceT $ runConduit $
         C.sourceDirectoryDeep True optsDistDir
      .| C.mapM (liftIO . matchFile processFile optsMatch)
      .| C.List.catMaybes
      .| C.List.consume
    let csv = BS.Lazy.UTF8.toString $ Csv.encode (sort stats)
    case optsOutput of
      Nothing -> putStrLn csv
      Just fp -> writeFile fp csv
