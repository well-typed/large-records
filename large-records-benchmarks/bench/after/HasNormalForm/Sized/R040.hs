#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module HasNormalForm.Sized.R040 where

import Data.Record.Generic
import Data.Record.Generic.Transform

import After.Sized.R0040

testInterpretTo :: ()
testInterpretTo = aux
  where
    aux :: HasNormalForm I R R => ()
    aux = ()
