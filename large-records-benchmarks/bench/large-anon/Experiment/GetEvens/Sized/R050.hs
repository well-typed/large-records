#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Experiment.GetEvens.Sized.R050 where

import Data.Record.Anon.Simple (Record)
import qualified Data.Record.Anon.Simple as Anon

import Bench.EvensOfSize.Evens050
import Common.RowOfSize.Row050

getEvens :: Record ExampleRow -> Evens
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
    }

