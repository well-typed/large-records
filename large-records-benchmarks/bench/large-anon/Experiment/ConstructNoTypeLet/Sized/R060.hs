#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Experiment.ConstructNoTypeLet.Sized.R060 where

import Data.Record.Anon.Simple (Record)

import Bench.Types
import Common.RowOfSize.Row060

record :: Word -> Record ExampleRow
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
      -- 30 .. 39
    , t30 = MkT x
    , t31 = MkT x
    , t32 = MkT x
    , t33 = MkT x
    , t34 = MkT x
    , t35 = MkT x
    , t36 = MkT x
    , t37 = MkT x
    , t38 = MkT x
    , t39 = MkT x
      -- 40 .. 49
    , t40 = MkT x
    , t41 = MkT x
    , t42 = MkT x
    , t43 = MkT x
    , t44 = MkT x
    , t45 = MkT x
    , t46 = MkT x
    , t47 = MkT x
    , t48 = MkT x
    , t49 = MkT x
      -- 50 .. 59
    , t50 = MkT x
    , t51 = MkT x
    , t52 = MkT x
    , t53 = MkT x
    , t54 = MkT x
    , t55 = MkT x
    , t56 = MkT x
    , t57 = MkT x
    , t58 = MkT x
    , t59 = MkT x
    }
