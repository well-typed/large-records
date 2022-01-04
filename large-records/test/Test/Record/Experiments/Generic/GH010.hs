{-# LANGUAGE CPP #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

-- {-# OPTIONS_GHC -ddump-ds-preopt -ddump-ds #-}

module Test.Record.Experiments.Generic.GH010 where

import Data.Aeson (Value)

import Test.Record.Experiments.Generic
import Test.Record.Experiments.HList
import Test.Record.Experiments.HList.HL010

hlistToJSON :: HList ExampleFields -> Value
hlistToJSON = gtoJSON