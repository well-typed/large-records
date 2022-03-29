#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module Experiment.SR_SetEvens.Sized.R070 where

import SuperRecord (Rec)
import qualified SuperRecord as SR

import Bench.EvensOfSize.Evens070
import Common.RowOfSize.Row070

setEvens :: Evens -> Rec Row -> Rec Row
setEvens Evens{..} r =
      -- 00 .. 09
      SR.set #t00 evens00
    . SR.set #t02 evens02
    . SR.set #t04 evens04
    . SR.set #t06 evens06
    . SR.set #t08 evens08
      -- 10 .. 19
    . SR.set #t10 evens10
    . SR.set #t12 evens12
    . SR.set #t14 evens14
    . SR.set #t16 evens16
    . SR.set #t18 evens18
      -- 20 .. 29
    . SR.set #t20 evens20
    . SR.set #t22 evens22
    . SR.set #t24 evens24
    . SR.set #t26 evens26
    . SR.set #t28 evens28
      -- 30 .. 39
    . SR.set #t30 evens30
    . SR.set #t32 evens32
    . SR.set #t34 evens34
    . SR.set #t36 evens36
    . SR.set #t38 evens38
      -- 40 .. 49
    . SR.set #t40 evens40
    . SR.set #t42 evens42
    . SR.set #t44 evens44
    . SR.set #t46 evens46
    . SR.set #t48 evens48
      -- 50 .. 59
    . SR.set #t50 evens50
    . SR.set #t52 evens52
    . SR.set #t54 evens54
    . SR.set #t56 evens56
    . SR.set #t58 evens58
      -- 60 .. 69
    . SR.set #t60 evens60
    . SR.set #t62 evens62
    . SR.set #t64 evens64
    . SR.set #t66 evens66
    . SR.set #t68 evens68
    $ r
