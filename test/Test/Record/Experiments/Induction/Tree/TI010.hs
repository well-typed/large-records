{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

-- {-# OPTIONS_GHC -ddump-ds-preopt #-}

module Test.Record.Experiments.Induction.Tree.TI010 where

import Data.Proxy

import Test.Record.Experiments.HList.HL010
import Test.Record.Experiments.Induction.Tree

requiresInstance :: ()
requiresInstance = requireEmptyClass (Proxy @ExampleFields)
