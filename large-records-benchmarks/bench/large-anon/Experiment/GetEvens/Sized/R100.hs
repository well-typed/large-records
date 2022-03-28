#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Experiment.GetEvens.Sized.R100 where

import Data.Record.Anonymous.Simple (Record)
import qualified Data.Record.Anonymous.Simple as Anon

import Bench.EvensOfSize.Evens100
import Common.RowOfSize.Row100

getEvens :: Record Row -> Evens
getEvens r = Evens {
      -- 00 .. 09
      evens00 = Anon.get #t00 r
    , evens02 = Anon.get #t02 r
    , evens04 = Anon.get #t04 r
    , evens06 = Anon.get #t06 r
    , evens08 = Anon.get #t08 r
      -- 10 .. 19
    , evens10 = Anon.get #t10 r
    , evens12 = Anon.get #t12 r
    , evens14 = Anon.get #t14 r
    , evens16 = Anon.get #t16 r
    , evens18 = Anon.get #t18 r
      -- 20 .. 29
    , evens20 = Anon.get #t20 r
    , evens22 = Anon.get #t22 r
    , evens24 = Anon.get #t24 r
    , evens26 = Anon.get #t26 r
    , evens28 = Anon.get #t28 r
      -- 30 .. 39
    , evens30 = Anon.get #t30 r
    , evens32 = Anon.get #t32 r
    , evens34 = Anon.get #t34 r
    , evens36 = Anon.get #t36 r
    , evens38 = Anon.get #t38 r
      -- 40 .. 49
    , evens40 = Anon.get #t40 r
    , evens42 = Anon.get #t42 r
    , evens44 = Anon.get #t44 r
    , evens46 = Anon.get #t46 r
    , evens48 = Anon.get #t48 r
      -- 50 .. 59
    , evens50 = Anon.get #t50 r
    , evens52 = Anon.get #t52 r
    , evens54 = Anon.get #t54 r
    , evens56 = Anon.get #t56 r
    , evens58 = Anon.get #t58 r
      -- 60 .. 69
    , evens60 = Anon.get #t60 r
    , evens62 = Anon.get #t62 r
    , evens64 = Anon.get #t64 r
    , evens66 = Anon.get #t66 r
    , evens68 = Anon.get #t68 r
      -- 70 .. 79
    , evens70 = Anon.get #t70 r
    , evens72 = Anon.get #t72 r
    , evens74 = Anon.get #t74 r
    , evens76 = Anon.get #t76 r
    , evens78 = Anon.get #t78 r
      -- 80 .. 89
    , evens80 = Anon.get #t80 r
    , evens82 = Anon.get #t82 r
    , evens84 = Anon.get #t84 r
    , evens86 = Anon.get #t86 r
    , evens88 = Anon.get #t88 r
      -- 90 .. 99
    , evens90 = Anon.get #t90 r
    , evens92 = Anon.get #t92 r
    , evens94 = Anon.get #t94 r
    , evens96 = Anon.get #t96 r
    , evens98 = Anon.get #t98 r
    }

