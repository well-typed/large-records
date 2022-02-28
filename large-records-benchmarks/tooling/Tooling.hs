{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tooling (
    -- * Common command line options
    parseDistDir
  , parseMatch
  , parseOutput
    -- * Running the tool
  , Parsed(..)
  , Label(..)
  , flattenLabel
  , matchFile
  ) where

import Data.List (intercalate)
import Options.Applicative
import Text.Regex.PCRE.Light

import qualified Data.ByteString.UTF8 as BS.UTF8
import qualified Data.Csv             as Csv
import qualified Data.Vector as V
import Data.Vector (Vector)

{-------------------------------------------------------------------------------
  Command line options shared by the different tools
-------------------------------------------------------------------------------}

parseDistDir :: Parser FilePath
parseDistDir = strOption $ mconcat [
      long "dist"
    , value "dist-newstyle"
    , showDefault
    , help "Dist directory"
    ]

parseMatch :: Parser Regex
parseMatch = option (readRegexp []) $ mconcat [
      long "match"
    , help $ concat [
          "Pattern match to apply (e.g. '.*dump-simpl'). "
        , "If you don't want the full filepath to be included in the output, "
        , "can use subpatterns such as '.*/(.*)/Sized/(.*).dump-*'."
        ]
    ]

parseOutput :: Parser (Maybe FilePath)
parseOutput = optional $ strOption $ mconcat [
      short 'o'
    , help "Output filename"
    ]

{-------------------------------------------------------------------------------
  Running the tool
-------------------------------------------------------------------------------}

data Parsed a = Parsed {
      parsedLabel :: Label
    , parsedData  :: a
    }
  -- The 'Ord' instance determines the ordering in the CSV file
  deriving (Show, Eq, Ord)

-- | Label used to identify the module
--
-- If the user specified subexpressions in their regexp, this will be the
-- concatenation of those subexpressions. Otherwise, it will be equal to
-- the filepath.
newtype Label = Label { getLabel ::[String] }
  deriving (Show, Eq, Ord)

flattenLabel :: Label -> String
flattenLabel = intercalate "/" . getLabel

instance Csv.ToRecord a => Csv.ToRecord (Parsed a) where
  toRecord Parsed{..} = V.concat [common, Csv.toRecord parsedData]
    where
      common :: Vector Csv.Field
      common = V.fromList $ map Csv.toField (getLabel parsedLabel)

matchFile ::
      (FilePath -> IO (Maybe a))
   -> Regex -> FilePath -> IO (Maybe (Parsed a))
matchFile f re fp =
    case match re (BS.UTF8.fromString fp) [] of
      Nothing ->
        -- No match, ignore
        pure Nothing
      Just [] ->
        -- Impossible
        error "matchFile: unexpected response from match"
      Just [_matched] -> do
        -- Matched, no subpatterns: use full path as label
        let label = Label [fp]
        fmap (Parsed label) <$> f fp
      Just (_matched : subs) -> do
        -- Specified one or more subpatterns
        let label = Label $ map BS.UTF8.toString subs
        fmap (Parsed label) <$> f fp

{-------------------------------------------------------------------------------
  Auxiliary: regexp utility
-------------------------------------------------------------------------------}

readRegexp :: [PCREOption] -> ReadM Regex
readRegexp opts = aux =<< str
  where
    aux :: String -> ReadM Regex
    aux s = either fail return $ compileM (BS.UTF8.fromString s) opts

