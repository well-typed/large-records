{-# LANGUAGE TypeApplications #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.ConstraintFamily_Deep.Sized.R020 where

import Data.Proxy

import Bench.HList
import Experiment.ConstraintFamily_Deep

import Common.HListOfSize.HL020

satisfyCF :: ()
satisfyCF = withCF (Proxy @(HList ExampleFields))
