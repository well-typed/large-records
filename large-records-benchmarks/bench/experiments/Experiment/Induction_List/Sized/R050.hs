{-# LANGUAGE TypeApplications #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.Induction_List.Sized.R050 where

import Data.Proxy

import Common.EmptyClass_List
import Common.HListOfSize.HL050

requiresInstance :: ()
requiresInstance = requireEmptyClass (Proxy @ExampleFields)
