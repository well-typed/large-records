#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_Unsafe.Sized.R020 where

import SuperRecord (Rec, (:=)(..))
import qualified SuperRecord as SR

import Bench.Types
import Common.RowOfSize.Row020 (Row)

record :: Word -> Rec Row
record x =
      -- 00 .. 09
      SR.unsafeRCons (#t00 := MkT x)
    $ SR.unsafeRCons (#t01 := MkT x)
    $ SR.unsafeRCons (#t02 := MkT x)
    $ SR.unsafeRCons (#t03 := MkT x)
    $ SR.unsafeRCons (#t04 := MkT x)
    $ SR.unsafeRCons (#t05 := MkT x)
    $ SR.unsafeRCons (#t06 := MkT x)
    $ SR.unsafeRCons (#t07 := MkT x)
    $ SR.unsafeRCons (#t08 := MkT x)
    $ SR.unsafeRCons (#t09 := MkT x)
      -- 10 .. 19
    $ SR.unsafeRCons (#t10 := MkT x)
    $ SR.unsafeRCons (#t11 := MkT x)
    $ SR.unsafeRCons (#t12 := MkT x)
    $ SR.unsafeRCons (#t13 := MkT x)
    $ SR.unsafeRCons (#t14 := MkT x)
    $ SR.unsafeRCons (#t15 := MkT x)
    $ SR.unsafeRCons (#t16 := MkT x)
    $ SR.unsafeRCons (#t17 := MkT x)
    $ SR.unsafeRCons (#t18 := MkT x)
    $ SR.unsafeRCons (#t19 := MkT x)
    $ SR.unsafeRNil 20
