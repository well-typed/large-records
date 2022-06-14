#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.HListLetAsCPS.Sized.R040 where

import TypeLet

import Bench.HList
import Bench.Types
import Common.HListOfSize.HL040

hlist :: HList Fields
hlist = constructLet $ \_ ->
    -- 39 .. 30
    letAs' (MkT 39 :* Nil)  $ \xs39 ->
    letAs' (MkT 38 :* xs39) $ \xs38 ->
    letAs' (MkT 37 :* xs38) $ \xs37 ->
    letAs' (MkT 36 :* xs37) $ \xs36 ->
    letAs' (MkT 35 :* xs36) $ \xs35 ->
    letAs' (MkT 34 :* xs35) $ \xs34 ->
    letAs' (MkT 33 :* xs34) $ \xs33 ->
    letAs' (MkT 32 :* xs33) $ \xs32 ->
    letAs' (MkT 31 :* xs32) $ \xs31 ->
    letAs' (MkT 30 :* xs31) $ \xs30 ->
    -- 29 .. 20
    letAs' (MkT 29 :* xs30) $ \xs29 ->
    letAs' (MkT 28 :* xs29) $ \xs28 ->
    letAs' (MkT 27 :* xs28) $ \xs27 ->
    letAs' (MkT 26 :* xs27) $ \xs26 ->
    letAs' (MkT 25 :* xs26) $ \xs25 ->
    letAs' (MkT 24 :* xs25) $ \xs24 ->
    letAs' (MkT 23 :* xs24) $ \xs23 ->
    letAs' (MkT 22 :* xs23) $ \xs22 ->
    letAs' (MkT 21 :* xs22) $ \xs21 ->
    letAs' (MkT 20 :* xs21) $ \xs20 ->
    -- 19 .. 10
    letAs' (MkT 19 :* xs20) $ \xs19 ->
    letAs' (MkT 18 :* xs19) $ \xs18 ->
    letAs' (MkT 17 :* xs18) $ \xs17 ->
    letAs' (MkT 16 :* xs17) $ \xs16 ->
    letAs' (MkT 15 :* xs16) $ \xs15 ->
    letAs' (MkT 14 :* xs15) $ \xs14 ->
    letAs' (MkT 13 :* xs14) $ \xs13 ->
    letAs' (MkT 12 :* xs13) $ \xs12 ->
    letAs' (MkT 11 :* xs12) $ \xs11 ->
    letAs' (MkT 10 :* xs11) $ \xs10 ->
    -- 09 .. 00
    letAs' (MkT 09 :* xs10) $ \xs09 ->
    letAs' (MkT 08 :* xs09) $ \xs08 ->
    letAs' (MkT 07 :* xs08) $ \xs07 ->
    letAs' (MkT 06 :* xs07) $ \xs06 ->
    letAs' (MkT 05 :* xs06) $ \xs05 ->
    letAs' (MkT 04 :* xs05) $ \xs04 ->
    letAs' (MkT 03 :* xs04) $ \xs03 ->
    letAs' (MkT 02 :* xs03) $ \xs02 ->
    letAs' (MkT 01 :* xs02) $ \xs01 ->
    letAs' (MkT 00 :* xs01) $ \xs00 ->
      castEqual xs00
