#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Experiment.ConstructNoTypeLet.Sized.R010 where

import Data.Record.Anonymous.Simple (Record)

import Bench.Types
import Common.RowOfSize.Row010

record :: Record Row
record = ANON {
      -- 00 .. 09
      t00 = MkT 00
    , t01 = MkT 01
    , t02 = MkT 02
    , t03 = MkT 03
    , t04 = MkT 04
    , t05 = MkT 05
    , t06 = MkT 06
    , t07 = MkT 07
    , t08 = MkT 08
    , t09 = MkT 09
    }