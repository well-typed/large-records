#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.SR_ToJSON.Sized.R070 where

import Data.Aeson (Value)
import SuperRecord (Rec, recToValue)

import Common.RowOfSize.Row070

recToJSON :: Rec Row -> Value
recToJSON = recToValue