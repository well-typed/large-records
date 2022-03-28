#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_Construct.Sized.R010 where

import SuperRecord

import Bench.Types
import Common.RowOfSize.Row010 (Row)

record :: Word -> Rec Row
record x =
      -- 00 .. 09
      rcons (#t00 := MkT x)
    $ rcons (#t01 := MkT x)
    $ rcons (#t02 := MkT x)
    $ rcons (#t03 := MkT x)
    $ rcons (#t04 := MkT x)
    $ rcons (#t05 := MkT x)
    $ rcons (#t06 := MkT x)
    $ rcons (#t07 := MkT x)
    $ rcons (#t08 := MkT x)
    $ rcons (#t09 := MkT x)
    $ rnil
