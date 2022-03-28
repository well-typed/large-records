#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Experiment.GetEvens.Sized.R010 where

import Data.Record.Anonymous.Simple (Record)
import qualified Data.Record.Anonymous.Simple as Anon

import Bench.EvensOfSize.Evens010
import Common.RowOfSize.Row010

getEvens :: Record Row -> Evens
getEvens r = Evens {
      -- 00 .. 09
      evens00 = Anon.get #t00 r
    , evens02 = Anon.get #t02 r
    , evens04 = Anon.get #t04 r
    , evens06 = Anon.get #t06 r
    , evens08 = Anon.get #t08 r
    }

