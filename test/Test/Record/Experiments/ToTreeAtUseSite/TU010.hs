{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

-- {-# OPTIONS_GHC -ddump-ds-preopt #-}

module Test.Record.Experiments.ToTreeAtUseSite.TU010 where

import Data.Proxy

import Test.Record.Experiments.HList.HL010
import Test.Record.Experiments.ToTreeAtUseSite
import Test.Record.Experiments.Util

requiresInstance :: ()

#ifdef BLOG2_VARIANT_QUADRATIC
requiresInstance = requireEmptyClass @(ToTree ExampleFields) Proxy
#else
requiresInstance = requireEmptyClass (Proxy @(ToTree ExampleFields))
#endif
