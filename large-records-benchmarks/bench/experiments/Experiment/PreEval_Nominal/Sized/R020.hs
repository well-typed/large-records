{-# LANGUAGE TypeApplications #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.PreEval_Nominal.Sized.R020 where

import Data.Proxy

import Common.EmptyClass_Tree_Nominal
import Common.HListOfSize.HL020

requiresInstance :: ()
requiresInstance = requireEmptyClass_preEval (Proxy @ExampleFields)
