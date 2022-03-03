#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.Generics_LR.Sized.R060 where

import Data.Aeson (Value)

import Infra.HList
import Experiment.Generics_LR

import Common.HListOfSize.HL060

hlistToJSON :: HList ExampleFields -> Value
hlistToJSON = gtoJSON