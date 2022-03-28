#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_GetEvens.Sized.R040 where

import SuperRecord (Rec)
import qualified SuperRecord as SR

import Bench.EvensOfSize.Evens040
import Common.RowOfSize.Row040

getEvens :: Rec Row -> Evens
getEvens r = Evens {
      -- 00 .. 09
      evens00 = SR.get #t00 r
    , evens02 = SR.get #t02 r
    , evens04 = SR.get #t04 r
    , evens06 = SR.get #t06 r
    , evens08 = SR.get #t08 r
      -- 10 .. 19
    , evens10 = SR.get #t10 r
    , evens12 = SR.get #t12 r
    , evens14 = SR.get #t14 r
    , evens16 = SR.get #t16 r
    , evens18 = SR.get #t18 r
      -- 20 .. 29
    , evens20 = SR.get #t20 r
    , evens22 = SR.get #t22 r
    , evens24 = SR.get #t24 r
    , evens26 = SR.get #t26 r
    , evens28 = SR.get #t28 r
      -- 30 .. 39
    , evens30 = SR.get #t30 r
    , evens32 = SR.get #t32 r
    , evens34 = SR.get #t34 r
    , evens36 = SR.get #t36 r
    , evens38 = SR.get #t38 r
    }

