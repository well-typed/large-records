#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module Experiment.SR_SetEvens.Sized.R030 where

import SuperRecord (Rec)
import qualified SuperRecord as SR

import Bench.EvensOfSize.Evens030
import Common.RowOfSize.Row030

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
    $ r
