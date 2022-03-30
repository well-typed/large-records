#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Experiment.ParseJSON.Sized.R090 where

import Data.Aeson (Value,)
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromJust)
import Data.Record.Anon.Simple (Record)
import Data.Record.Generic.JSON (gparseJSON)

import Common.RowOfSize.Row090

recFromJSON :: Value -> Record ExampleRow
recFromJSON = fromJust . parseMaybe gparseJSON