#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Experiment.ConstructNoTypeLet.Sized.R030 where

import Data.Record.Anonymous.Simple (Record)

import Bench.Types
import Common.RowOfSize.Row030

record :: Word -> Record Row
record x = ANON {
      -- 00 .. 09
      t00 = MkT x
    , t01 = MkT x
    , t02 = MkT x
    , t03 = MkT x
    , t04 = MkT x
    , t05 = MkT x
    , t06 = MkT x
    , t07 = MkT x
    , t08 = MkT x
    , t09 = MkT x
      -- 10 .. 19
    , t10 = MkT x
    , t11 = MkT x
    , t12 = MkT x
    , t13 = MkT x
    , t14 = MkT x
    , t15 = MkT x
    , t16 = MkT x
    , t17 = MkT x
    , t18 = MkT x
    , t19 = MkT x
      -- 20 .. 29
    , t20 = MkT x
    , t21 = MkT x
    , t22 = MkT x
    , t23 = MkT x
    , t24 = MkT x
    , t25 = MkT x
    , t26 = MkT x
    , t27 = MkT x
    , t28 = MkT x
    , t29 = MkT x
    }