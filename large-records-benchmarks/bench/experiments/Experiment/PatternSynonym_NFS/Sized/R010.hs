{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.PatternSynonym_NFS.Sized.R010 where

import Bench.Types

data R

viewR :: R -> (
      (T 00, T 01, T 02, T 03, T 04, T 05, T 06, T 07, T 08, T 09)
    )
viewR = undefined

pattern MkR ::
     T 00 -> T 01 -> T 02 -> T 03 -> T 04 -> T 05 -> T 06 -> T 07 -> T 08 -> T 09
  -> R
pattern MkR {
      x00, x01, x02, x03, x04, x05, x06, x07, x08, x09
    } <- (viewR -> (
             (x00, x01, x02, x03, x04, x05, x06, x07, x08, x09)
           )
         )

