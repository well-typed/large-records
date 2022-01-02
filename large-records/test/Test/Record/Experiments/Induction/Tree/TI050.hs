{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

module Test.Record.Experiments.Induction.Tree.TI050 where

import Data.Proxy

import Test.Record.Experiments.HList.HL050
import Test.Record.Experiments.Induction.Tree

requiresInstance :: ()
requiresInstance = requireEmptyClass (Proxy @ExampleFields)
