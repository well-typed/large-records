{-# LANGUAGE CPP #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

module Test.Record.Experiments.Generic.GH040 where

import Data.Aeson (Value)

import Test.Record.Experiments.Generic
import Test.Record.Experiments.HList
import Test.Record.Experiments.HList.HL040

hlistToJSON :: HList ExampleFields -> Value
hlistToJSON = gtoJSON