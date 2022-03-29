#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.SR_ParseJSON.Sized.R050 where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromJust)
import SuperRecord (Rec, recJsonParser)

import Common.RowOfSize.Row050

recFromJSON :: Value -> Rec Row
recFromJSON = fromJust . parseMaybe recJsonParser

