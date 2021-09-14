{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

-- {-# OPTIONS_GHC -ddump-ds-preopt -ddump-ds -ddump-simpl #-}

module Test.Record.Experiments.Induction.List.LI010 where

import Data.Proxy

import Test.Record.Experiments.Induction.List
import Test.Record.Experiments.HList.HL010

requiresInstance :: ()
requiresInstance = requireEmptyClass (Proxy @ExampleFields)
