#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_GetEvens.Sized.R010 where

import SuperRecord (Rec)
import qualified SuperRecord as SR

import Bench.EvensOfSize.Evens010
import Common.RowOfSize.Row010

getEvens :: Rec Row -> Evens
getEvens r = Evens {
      -- 00 .. 09
      evens00 = SR.get #t00 r
    , evens02 = SR.get #t02 r
    , evens04 = SR.get #t04 r
    , evens06 = SR.get #t06 r
    , evens08 = SR.get #t08 r
    }

