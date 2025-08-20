module Bench.Util.Criterion (
    envWHNF
  , envPureWHNF
  , envPureNF
  ) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Criterion

-- | Internal auxiliary to envWHNF
newtype WHNF a = WHNF { getWHNF :: a }

instance NFData (WHNF a) where
  rnf (WHNF a) = a `seq` ()

-- | Variation on 'env' that only forces the environment to WHNF
envWHNF :: IO env -> (env -> Benchmark) -> Benchmark
envWHNF mkEnv useEnv = env (WHNF <$> mkEnv) (useEnv . getWHNF)

envPureWHNF :: env -> (env -> Benchmark) -> Benchmark
envPureWHNF = envWHNF . evaluate

envPureNF :: NFData env => env -> (env -> Benchmark) -> Benchmark
envPureNF = env . evaluate
