#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_Construct.Sized.R010 where

import SuperRecord (Rec, (:=)(..))
import qualified SuperRecord as SR

import Bench.Types
import Common.RowOfSize.Row010 (Row)

record :: Word -> Rec Row
record x =
      -- 00 .. 09
      SR.rcons (#t00 := MkT x)
    $ SR.rcons (#t01 := MkT x)
    $ SR.rcons (#t02 := MkT x)
    $ SR.rcons (#t03 := MkT x)
    $ SR.rcons (#t04 := MkT x)
    $ SR.rcons (#t05 := MkT x)
    $ SR.rcons (#t06 := MkT x)
    $ SR.rcons (#t07 := MkT x)
    $ SR.rcons (#t08 := MkT x)
    $ SR.rcons (#t09 := MkT x)
    $ SR.rnil
