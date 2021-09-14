{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleInstances #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Record.Experiments.Induction.Tree.TI100 where

import Data.Proxy

import Test.Record.Experiments.HList.HL100
import Test.Record.Experiments.Induction.Tree

requiresInstance :: ()
requiresInstance = requireEmptyClass (Proxy @ExampleFields)
