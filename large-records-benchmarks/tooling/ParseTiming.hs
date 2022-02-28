{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Conduit
import Data.Bifunctor
import Data.List (sort, isPrefixOf)
import Data.Map (Map)
import Data.Set (Set)
import Options.Applicative hiding (columns)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex.PCRE.Light (Regex)

import qualified Data.ByteString.Lazy.UTF8 as BS.Lazy.UTF8
import qualified Data.ByteString.UTF8      as BS.UTF8
import qualified Data.Conduit.Combinators  as C
import qualified Data.Conduit.List         as C.List
import qualified Data.Csv                  as Csv
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import qualified Data.Vector               as V

import Tooling
import Data.ByteString.Lazy (ByteString)

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

      -- | Omit the per-phase, showing totals only
    , optsOmitPerPhase :: Bool
    }

parseOptions :: Parser Options
parseOptions = Options
    <$> parseDistDir
    <*> parseMatch
    <*> parseOutput
    <*> (switch $ mconcat [
            long "omit-per-phase"
          , help "Omit per-phase info, showing only totals"
          ])

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (parseOptions <**> helper) $ mconcat [
        fullDesc
      , progDesc $ concat [
            "Process -ddump-timings files. "
          , "Note: we currently only output timing information, as the "
          , "allocation info from the timings reports is currently not very "
          , "meaningful."
          ]
      ]

{-------------------------------------------------------------------------------
  Timings
-------------------------------------------------------------------------------}

data Timings = Timings {
      timingsTotal :: AllocAndTime
    , timingsMap   :: Map Phase AllocAndTime
    }
  -- 'Ord' instance dictates ordering in the CSV file
  deriving (Show, Eq, Ord)

data AllocAndTime = AllocAndTime {
      alloc :: Int
    , time  :: Double
    }
  deriving (Show, Eq, Ord)

instance Monoid AllocAndTime where
  mempty = AllocAndTime 0 0

instance Semigroup AllocAndTime where
  AllocAndTime t a <> AllocAndTime t' a' = AllocAndTime (t + t') (a + a')

-- | Construct 'Timings' from the parsed lines
mkTimings :: [(Phase, AllocAndTime)] -> Timings
mkTimings entries = Timings{..}
  where
    timingsMap   = Map.fromListWith (<>) entries
    timingsTotal = mconcat $ map snd entries

type Phase = String

-- | Parse a line from the timings file
--
--  These lines look something like
--
-- > Parser [Main]: alloc=114488 time=0.076
--
-- We omit the the module, as we parse this /per/ module
parseLine :: String -> (Phase, AllocAndTime)
parseLine str0
  | Just alloc' <- readMaybe (drop 6 alloc)
  , Just time'  <- readMaybe (drop 5 time)
  , "alloc=" `isPrefixOf` alloc
  , "time="  `isPrefixOf` time
  = (phase, AllocAndTime { alloc = alloc', time = time' })

  | otherwise
  = error $ "parseLine: could not parse " ++ show str0
         ++ ": " ++ show (phase, modl, alloc, time)
  where
    (phase , str1) = break (== '[') str0
    (modl  , str2) = break (== ']') (drop 1 str1)
    (alloc , str3) = break (== ' ') (drop 3 str2)
    time           = drop 1 str3

parseTimings :: String -> Timings
parseTimings =
      mkTimings
    . map parseLine
    . filter (\line -> not ("systool" `isPrefixOf` line))
    . lines

{-------------------------------------------------------------------------------
  Global view

  Once we have parsed all modules, we can figure out which phases we have.
-------------------------------------------------------------------------------}

allPhases :: [Parsed Timings] -> Set Phase
allPhases = Set.unions . map (Map.keysSet . timingsMap . parsedData)

-- | Construct named record (with headers)
--
-- We use generate a CSV file with headers only if we show all phases. In this
-- case we will flatten the module label.
--
-- See also 'toUnnamedRecord'.
toNamedRecord :: [Phase] -> Parsed Timings -> Csv.NamedRecord
toNamedRecord phases Parsed{..} =
    HM.fromList . map (bimap BS.UTF8.fromString BS.UTF8.fromString) $
        ("module"     , flattenLabel parsedLabel)
      : ("total time" , showTime $ time timingsTotal)
      : map aux phases
  where
    Timings{..} = parsedData

    aux :: Phase -> (String, String)
    aux phase = case Map.lookup phase timingsMap of
                  Just at -> (phase, showTime (time at))
                  Nothing -> (phase, "0") -- Phase didn't happen in this module

-- | Construct unnamed record (no headers, don't flatten labels)
--
-- We use this when the 'optsOmitPerPhase' is specified.
toUnnamedRecord :: Parsed Timings -> Csv.Record
toUnnamedRecord Parsed{..} =
    V.fromList . map BS.UTF8.fromString $
      getLabel parsedLabel ++ [showTime $ time timingsTotal]
  where
    Timings{..} = parsedData

showTime :: Double -> String
showTime = printf "%0.3f"

toCSV :: Options -> [Phase] -> [Parsed Timings] -> ByteString
toCSV Options{optsOmitPerPhase = False} phases timings =
    Csv.encodeByName columns $ map (toNamedRecord phases) timings
  where
    columns :: Csv.Header
    columns = V.fromList $
                 "module"
               : "total time"
               : map BS.UTF8.fromString phases
toCSV Options{optsOmitPerPhase = True} _phases timings =
    Csv.encode $ map toUnnamedRecord timings

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

processFile :: FilePath -> IO (Maybe Timings)
processFile fp = do
    contents <- readFile fp
    return $ Just (parseTimings contents)

main :: IO ()
main = do
    opts@Options{..} <- getOptions
    timings <- runResourceT $ runConduit $
         C.sourceDirectoryDeep True optsDistDir
      .| C.mapM (liftIO . matchFile processFile optsMatch)
      .| C.List.catMaybes
      .| C.List.consume

    let phases :: [Phase]
        phases = Set.toList $ allPhases timings

        csv :: String
        csv = BS.Lazy.UTF8.toString $ toCSV opts phases (sort timings)

    case optsOutput of
      Nothing -> putStrLn csv
      Just fp -> writeFile fp csv
