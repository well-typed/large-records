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

module Test.Record.Experiments.PatternSynonym.PS030 where

import Test.Record.Size.Infra

data R

viewR :: R -> (
      (T 00, T 01, T 02, T 03, T 04, T 05, T 06, T 07, T 08, T 09)
    , (T 10, T 11, T 12, T 13, T 14, T 15, T 16, T 17, T 18, T 19)
    , (T 20, T 21, T 22, T 23, T 24, T 25, T 26, T 27, T 28, T 29)
    )
viewR = undefined

pattern MkR ::
     T 00 -> T 01 -> T 02 -> T 03 -> T 04 -> T 05 -> T 06 -> T 07 -> T 08 -> T 09
  -> T 10 -> T 11 -> T 12 -> T 13 -> T 14 -> T 15 -> T 16 -> T 17 -> T 18 -> T 19
  -> T 20 -> T 21 -> T 22 -> T 23 -> T 24 -> T 25 -> T 26 -> T 27 -> T 28 -> T 29
  -> R
pattern MkR {
      x00, x01, x02, x03, x04, x05, x06, x07, x08, x09
    , x10, x11, x12, x13, x14, x15, x16, x17, x18, x19
    , x20, x21, x22, x23, x24, x25, x26, x27, x28, x29
    } <- (viewR -> (
             (x00, x01, x02, x03, x04, x05, x06, x07, x08, x09)
           , (x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
           , (x20, x21, x22, x23, x24, x25, x26, x27, x28, x29)
           )
         )

