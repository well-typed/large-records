#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_GetEvens.Sized.R050 where

import SuperRecord (Rec)
import qualified SuperRecord as SR

import Bench.EvensOfSize.Evens050
import Common.RowOfSize.Row050

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
      -- 40 .. 49
    , evens40 = SR.get #t40 r
    , evens42 = SR.get #t42 r
    , evens44 = SR.get #t44 r
    , evens46 = SR.get #t46 r
    , evens48 = SR.get #t48 r
    }

