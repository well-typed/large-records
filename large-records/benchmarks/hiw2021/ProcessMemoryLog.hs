{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProcessMemoryLog where

import Data.Bifunctor
import Data.List (isSuffixOf, intercalate, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

processMemoryLog :: IO ()
processMemoryLog = do
    parsed <- parse <$> readFile "memory.log"
    writeFile "parsed.log" $ show parsed
    let (summaryTime, summaryMemory) = summariseLog parsed
    writeFile "time.csv"   $ toCSV summaryTime
    writeFile "memory.csv" $ toCSV summaryMemory

{-------------------------------------------------------------------------------
  Generate CSV
-------------------------------------------------------------------------------}

toCSV :: forall a. Show a => [(FilePath, Summary a)] -> String
toCSV = unlines . map (uncurry genLine)
  where
    genLine :: FilePath -> Summary a -> String
    genLine fp Summary{mean, stdErr} = intercalate "\t" [
          fp
        , show mean
        , show stdErr
        ]

{-------------------------------------------------------------------------------
  Processing
-------------------------------------------------------------------------------}

summariseLog ::
     MemoryLog
  -> ( [(FilePath, Summary ExecutionTime)]
     , [(FilePath, Summary MemoryUsage)]
     )
summariseLog memoryLog =
    ( map (second (summarise  baselineTime   . fst)) memoryLog
    , map (second (summarise' baselineMemory . snd)) memoryLog
    )
  where
    baselineTime :: [ExecutionTime]
    baselineTime =
        fst . fromJust $
          lookup "test/Test/Record/Size/Before/Baseline.hs" memoryLog

    baselineMemory :: [MemoryUsage]
    baselineMemory =
        snd . fromJust $
          lookup "test/Test/Record/Size/Before/Baseline.hs" memoryLog

{-------------------------------------------------------------------------------
  Mean and standard deviation, corrected for the baseline

  By a slight abuse of notation we'll let X and Y range over two random
  variables, as well as over two samples /drawn/ from those random variables.
  We'll assume the sample size (for both variables) is N.

  * Basic definitions:

  > mean X = sum X_i                   /  N         (mean aka average)
  > var  X = sum ((X_i - mean X) ** 2) / (N - 1)    (variance)
  > std  X = sqrt (var X)                           (standard deviation)

  See <https://en.wikipedia.org/wiki/Standard_deviation>

  * Correlation between two variables:

  > cov X Y = sum ((X_i - mean X) * (Y_i - mean Y)) / (N - 1)

  Notice that

  > cov X X = sum ((X_i - mean X) * (X_i - mean X)) / (N - 1)
  >         = sum ((X_i - mean X) ** 2) / (N - 1)
  >         = var X

  See <https://en.wikipedia.org/wiki/Covariance#Covariance_with_itself>

  * Combining and scaling variables

  For two random variables X and Y, we have

  > mean (X + Y) = mean X + mean Y
  > var  (X + Y) = var  X + var  Y - 2 * cov X Y

  Moreover

  > mean (a * X)         =  a       * mean X
  > var  (a * X)         = (a ** 2) * var  X
  > cov  (a * X) (b * Y) = (a * b)  * cov  X Y

  See <https://en.wikipedia.org/wiki/Covariance#Covariance_of_linear_combinations>

  * Correcting for the baseline

  To correct for the baseline, we are interested in

  > X - Y = X + -1 * Y

  Applying the above definitions, we get

  >   mean (X - Y)
  > = mean (X + (-1 * Y))
  > = mean X + mean (-1 * Y)
  > = mean X + (-1 * mean Y)
  > = mean X - mean Y

  >   var (X - Y)
  > = var (X + (-1 * Y))
  > = var X + var (-1 * Y) - 2 * cov X (-1 * Y)
  > = var X + var Y        - 2 * (-1 * cov X Y)
  > = var X + var Y        - 2 * cov X Y

  Notice that if we let Y = X, then

  > mean (X - X) = mean X - mean X = 0
  > var  (X - X) = var X + var X - 2 * cov X X = var X + var X - 2 * var X = 0

  as expected: if we think of X as the "baseline", then correcting the baseline
  "for itself" yields an average of zero and no variance.

  With many thanks to Lars Brünjes for explaining all this :)
-------------------------------------------------------------------------------}

-- | Mean value and standard deviation, both corrected for the baseline
data Summary a = Summary {
      -- | Mean (average)
      mean :: a

      -- | Standard deviation
      --
      -- The standard deviation indicates how much a new measurement is likely
      -- to diverge from the mean.
    , stdDev :: a

      -- | Standard error
      --
      -- The standard error indicates how much the TRUE mean is likely to be
      -- away from your experimental mean.
      --
      -- See <https://www.investopedia.com/ask/answers/042415/what-difference-between-standard-error-means-and-standard-deviation.asp>
    , stdErr :: a
    }
  deriving (Functor)

summarise ::
     [Double] -- ^ Baseline (X)
  -> [Double] -- ^ Sample (Y)
  -> Summary Double
summarise ys xs = Summary {mean, stdDev, stdErr}
  where
    n :: Double
    n = if length xs == length ys
          then fromIntegral (length xs)
          else error "summarise: not same sample size"

    mean, stdDev, stdErr :: Double
    mean   = mean_X - mean_Y
    stdDev = sqrt (var_X + var_Y - 2 * cov_X_Y)
    stdErr = stdDev / sqrt n

    mean_X, mean_Y :: Double
    mean_X = sum xs / n
    mean_Y = sum ys / n

    var_X, var_Y :: Double
    var_X = sumOver (\x -> (x - mean_X) ** 2) xs
          / (n - 1)
    var_Y = sumOver (\y -> (y - mean_Y) ** 2) ys
          / (n - 1)

    cov_X_Y :: Double
    cov_X_Y = sumOver (\(x, y) -> (x - mean_X) * (y - mean_Y)) (zip xs ys)
            / (n - 1)

    sumOver :: Num b => (a -> b) -> [a] -> b
    sumOver f = sum . map f

summarise' :: [Int] -> [Int] -> Summary Int
summarise' xs ys =
    round <$> summarise (map fromIntegral xs) (map fromIntegral ys)

{-------------------------------------------------------------------------------
  Parsing the log
-------------------------------------------------------------------------------}

type MemoryLog = [(FilePath, ([ExecutionTime], [MemoryUsage]))]

-- Elapsed real time (wall clock) in seconds
type ExecutionTime = Double

-- Maximum resident set size in KB
type MemoryUsage = Int

parse :: String -> MemoryLog
parse =
      map (second (unzip . map snd))
    . groupEq fst
    . sortBy (comparing fst)
    . map (parseEntry . bimap (splitOn ' ') (splitOn '\t'))
    . pairUp
    . lines
  where
    parseEntry :: ([String], [String]) -> (String, (Double, Int))
    parseEntry ([_ix, fp], [time, mem]) = (fp, (read time, read mem))
    parseEntry entry = error $ "parseEntry: unrecognized entry " ++ show entry

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

pairUp :: [a] -> [(a, a)]
pairUp []         = []
pairUp [_]        = error "pairUp: not an even number of elements"
pairUp (x1:x2:xs) = (x1, x2) : pairUp xs

splitOn :: forall a. Eq a => a -> [a] -> [[a]]
splitOn sep = go []
  where
    go :: [a] -> [a] -> [[a]]
    go acc []     = [reverse acc]
    go acc (x:xs)
      | x == sep  = reverse acc : go [] xs
      | otherwise = go (x:acc) xs

-- | Variation on 'groupBy' that also records the canonical value per group
groupEq :: forall a b. Eq b => (a -> b) -> [a] -> [(b, [a])]
groupEq f = \case
    []   -> []
    a:as -> go (f a) [a] as
  where
    go :: b -> [a] -> [a] -> [(b, [a])]
    go b acc []     = [(b, reverse acc)]
    go b acc (a:as)
        | f a == b  = go b (a:acc) as
        | otherwise = (b, reverse acc) : go b' [a] as
      where
        b' = f a
