#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.Generics_SOP.Sized.R050 where

import Data.Aeson (Value)

import Bench.HList
import Experiment.Generics_SOP

import Common.HListOfSize.HL050

hlistToJSON :: HList ExampleFields -> Value
hlistToJSON = gtoJSON