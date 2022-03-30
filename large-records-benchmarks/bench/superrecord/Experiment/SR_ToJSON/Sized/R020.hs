#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.SR_ToJSON.Sized.R020 where

import Data.Aeson (Value)
import SuperRecord (Rec, recToValue)

import Common.RowOfSize.Row020

recToJSON :: Rec ExampleRow -> Value
recToJSON = recToValue