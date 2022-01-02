{-# LANGUAGE CPP #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

module Test.Record.Experiments.Generic.GH020 where

import Data.Aeson (Value)

import Test.Record.Experiments.Generic
import Test.Record.Experiments.HList
import Test.Record.Experiments.HList.HL020

hlistToJSON :: HList ExampleFields -> Value
hlistToJSON = gtoJSON