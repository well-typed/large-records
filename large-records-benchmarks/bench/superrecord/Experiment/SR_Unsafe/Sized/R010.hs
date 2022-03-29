#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_Unsafe.Sized.R010 where

import SuperRecord (Rec, (:=)(..))
import qualified SuperRecord as SR

import Bench.Types
import Common.RowOfSize.Row010 (Row)

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
    $ SR.unsafeRNil 10
