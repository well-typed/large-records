#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Experiment.ToJSON.Sized.R010 where

import Data.Aeson (Value)
import Data.Record.Generic.JSON (gtoJSON)

import Common.RowOfSize.Row010
import Data.Record.Anonymous.Simple (Record)

recToJSON :: Record Row -> Value
recToJSON = gtoJSON