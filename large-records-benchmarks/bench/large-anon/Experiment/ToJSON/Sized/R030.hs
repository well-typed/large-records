#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Experiment.ToJSON.Sized.R030 where

import Data.Aeson (Value)
import Data.Record.Generic.JSON (gtoJSON)

import Common.RowOfSize.Row030
import Data.Record.Anon.Simple (Record)

recToJSON :: Record ExampleRow -> Value
recToJSON = gtoJSON
