#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Experiment.SetEvens.Sized.R080 where

import Data.Record.Anonymous.Simple (Record)
import qualified Data.Record.Anonymous.Simple as Anon

import Bench.EvensOfSize.Evens080
import Common.RowOfSize.Row080

setEvens :: Evens -> Record Row -> Record Row
setEvens Evens{..} r =
    -- 00 .. 09
    Anon.set #t00 evens00
  . Anon.set #t02 evens02
  . Anon.set #t04 evens04
  . Anon.set #t06 evens06
  . Anon.set #t08 evens08
    -- 10 .. 19
  . Anon.set #t10 evens10
  . Anon.set #t12 evens12
  . Anon.set #t14 evens14
  . Anon.set #t16 evens16
  . Anon.set #t18 evens18
    -- 20 .. 29
  . Anon.set #t20 evens20
  . Anon.set #t22 evens22
  . Anon.set #t24 evens24
  . Anon.set #t26 evens26
  . Anon.set #t28 evens28
    -- 30 .. 39
  . Anon.set #t30 evens30
  . Anon.set #t32 evens32
  . Anon.set #t34 evens34
  . Anon.set #t36 evens36
  . Anon.set #t38 evens38
    -- 40 .. 49
  . Anon.set #t40 evens40
  . Anon.set #t42 evens42
  . Anon.set #t44 evens44
  . Anon.set #t46 evens46
  . Anon.set #t48 evens48
    -- 50 .. 59
  . Anon.set #t50 evens50
  . Anon.set #t52 evens52
  . Anon.set #t54 evens54
  . Anon.set #t56 evens56
  . Anon.set #t58 evens58
    -- 60 .. 69
  . Anon.set #t60 evens60
  . Anon.set #t62 evens62
  . Anon.set #t64 evens64
  . Anon.set #t66 evens66
  . Anon.set #t68 evens68
    -- 70 .. 79
  . Anon.set #t70 evens70
  . Anon.set #t72 evens72
  . Anon.set #t74 evens74
  . Anon.set #t76 evens76
  . Anon.set #t78 evens78
  $ r

