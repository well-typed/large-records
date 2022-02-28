{-# LANGUAGE TypeApplications #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.PreEval_Phantom.Sized.R080 where

import Data.Proxy

import Common.EmptyClass_Tree_Phantom
import Common.HListOfSize.HL080

requiresInstance :: ()
requiresInstance = requireEmptyClass_preEval (Proxy @ExampleFields)
