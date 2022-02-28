{-# LANGUAGE TypeApplications #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.ConstraintFamily_Shallow.Sized.R090 where

import Data.Proxy

import Experiment.ConstraintFamily_Shallow
import Infra.HList

import Common.HListOfSize.HL090

satisfyCF :: ()
satisfyCF = withCF (Proxy @(HList ExampleFields))