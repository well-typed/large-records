#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_GetEvens.Sized.R090 where

import SuperRecord (Rec)
import qualified SuperRecord as SR

import Bench.EvensOfSize.Evens090
import Common.RowOfSize.Row090

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
      -- 50 .. 59
    , evens50 = SR.get #t50 r
    , evens52 = SR.get #t52 r
    , evens54 = SR.get #t54 r
    , evens56 = SR.get #t56 r
    , evens58 = SR.get #t58 r
      -- 60 .. 69
    , evens60 = SR.get #t60 r
    , evens62 = SR.get #t62 r
    , evens64 = SR.get #t64 r
    , evens66 = SR.get #t66 r
    , evens68 = SR.get #t68 r
      -- 70 .. 79
    , evens70 = SR.get #t70 r
    , evens72 = SR.get #t72 r
    , evens74 = SR.get #t74 r
    , evens76 = SR.get #t76 r
    , evens78 = SR.get #t78 r
      -- 80 .. 89
    , evens80 = SR.get #t80 r
    , evens82 = SR.get #t82 r
    , evens84 = SR.get #t84 r
    , evens86 = SR.get #t86 r
    , evens88 = SR.get #t88 r
    }

