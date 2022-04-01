#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module Experiment.SR_SetEvens.Sized.R010 where

import SuperRecord (Rec)
import qualified SuperRecord as SR

import Bench.EvensOfSize.Evens010
import Common.RowOfSize.Row010

setEvens :: Evens -> Rec ExampleRow -> Rec ExampleRow
setEvens Evens{..} r =
      -- 00 .. 09
      SR.set #t00 evens00
    . SR.set #t02 evens02
    . SR.set #t04 evens04
    . SR.set #t06 evens06
    . SR.set #t08 evens08
    $ r
