#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.SR_ToJSON.Sized.R080 where

import Data.Aeson (Value)
import SuperRecord (Rec, recToValue)

import Common.RowOfSize.Row080

recToJSON :: Rec ExampleRow -> Value
recToJSON = recToValue