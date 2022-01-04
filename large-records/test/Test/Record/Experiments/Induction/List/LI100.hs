{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleInstances #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

{-# OPTIONS_GHC -Wno-orphans #-}

-- {-# OPTIONS_GHC -O -ddump-ds-preopt -ddump-ds -ddump-simpl #-}

module Test.Record.Experiments.Induction.List.LI100 where

import Data.Proxy

import Test.Record.Experiments.Induction.List
import Test.Record.Experiments.HList.HL100

-- instance {-# OVERLAPPING #-} EmptyClass ExampleFields

requiresInstance :: ()
requiresInstance = requireEmptyClass (Proxy @ExampleFields)
