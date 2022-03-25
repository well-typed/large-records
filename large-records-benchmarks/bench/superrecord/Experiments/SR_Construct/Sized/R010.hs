#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiments.SR_Construct.Sized.R010 where

import SuperRecord

import Bench.Types
import Common.RowOfSize.Row010 (Row)

record :: Rec Row
record =
      -- 00 .. 09
      rcons (#t00 := MkT 00)
    $ rcons (#t01 := MkT 01)
    $ rcons (#t02 := MkT 02)
    $ rcons (#t03 := MkT 03)
    $ rcons (#t04 := MkT 04)
    $ rcons (#t05 := MkT 05)
    $ rcons (#t06 := MkT 06)
    $ rcons (#t07 := MkT 07)
    $ rcons (#t08 := MkT 08)
    $ rcons (#t09 := MkT 09)
    $ rnil
