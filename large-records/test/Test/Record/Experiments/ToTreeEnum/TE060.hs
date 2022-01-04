{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

module Test.Record.Experiments.ToTreeEnum.TE060 where

import Data.Proxy

import Test.Record.Experiments.HList.HL060
import Test.Record.Experiments.ToTreeEnum

requiresInstance :: ()
requiresInstance = requireEmptyClass (Proxy @ExampleFields)
