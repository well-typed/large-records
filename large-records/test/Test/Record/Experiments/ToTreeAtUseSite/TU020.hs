{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

module Test.Record.Experiments.ToTreeAtUseSite.TU020 where

import Data.Proxy

import Test.Record.Experiments.HList.HL020
import Test.Record.Experiments.ToTreeAtUseSite
import Test.Record.Experiments.Util

requiresInstance :: ()

#ifdef BLOG2_VARIANT_QUADRATIC
requiresInstance = requireEmptyClass @(ToTree ExampleFields) Proxy
#else
requiresInstance = requireEmptyClass (Proxy @(ToTree ExampleFields))
#endif
