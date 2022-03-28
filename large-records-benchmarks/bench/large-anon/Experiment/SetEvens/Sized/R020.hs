#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Experiment.SetEvens.Sized.R020 where

import Data.Record.Anonymous.Simple (Record)
import qualified Data.Record.Anonymous.Simple as Anon

import Bench.EvensOfSize.Evens020
import Common.RowOfSize.Row020

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
  $ r

