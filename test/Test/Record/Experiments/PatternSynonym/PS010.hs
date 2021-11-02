{-# LANGUAGE CPP             #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

#if NOFIELDSELECTORS
{-# LANGUAGE NoFieldSelectors #-}
#endif

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

{-# OPTIONS_GHC -ddump-simpl #-}

module Test.Record.Experiments.PatternSynonym.PS010 where

import Test.Record.Size.Infra

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

