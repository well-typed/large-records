#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Experiment.SetEvens.Sized.R040 where

import Data.Record.Anon.Simple (Record)
import qualified Data.Record.Anon.Simple as Anon

import Bench.EvensOfSize.Evens040
import Common.RowOfSize.Row040

setEvens :: Evens -> Record ExampleRow -> Record ExampleRow
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
  $ r

