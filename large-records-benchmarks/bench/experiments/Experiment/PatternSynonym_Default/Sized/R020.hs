{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.PatternSynonym_Default.Sized.R020 where

import Bench.Types

data R

viewR :: R -> (
      (T 00, T 01, T 02, T 03, T 04, T 05, T 06, T 07, T 08, T 09)
    , (T 10, T 11, T 12, T 13, T 14, T 15, T 16, T 17, T 18, T 19)
    )
viewR = undefined

pattern MkR ::
     T 00 -> T 01 -> T 02 -> T 03 -> T 04 -> T 05 -> T 06 -> T 07 -> T 08 -> T 09
  -> T 10 -> T 11 -> T 12 -> T 13 -> T 14 -> T 15 -> T 16 -> T 17 -> T 18 -> T 19
  -> R
pattern MkR {
      x00, x01, x02, x03, x04, x05, x06, x07, x08, x09
    , x10, x11, x12, x13, x14, x15, x16, x17, x18, x19
    } <- (viewR -> (
             (x00, x01, x02, x03, x04, x05, x06, x07, x08, x09)
           , (x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
           )
         )

