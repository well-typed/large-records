#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Experiment.SetEvens.Sized.R010 where

import Data.Record.Anon.Simple (Record)
import qualified Data.Record.Anon.Simple as Anon

import Bench.EvensOfSize.Evens010
import Common.RowOfSize.Row010

setEvens :: Evens -> Record ExampleRow -> Record ExampleRow
setEvens Evens{..} r =
    -- 00 .. 09
    Anon.set #t00 evens00
  . Anon.set #t02 evens02
  . Anon.set #t04 evens04
  . Anon.set #t06 evens06
  . Anon.set #t08 evens08
  $ r

