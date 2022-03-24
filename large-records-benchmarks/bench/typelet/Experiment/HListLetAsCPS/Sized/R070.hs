#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.HListLetAsCPS.Sized.R070 where

import TypeLet

import Bench.HList
import Bench.Types
import Common.HListOfSize.HL070

hlist :: HList Fields
hlist = letT' Proxy $ \(_ :: Proxy r) -> castEqual $
    -- 69 .. 60
    letAs' @(HList r) (MkT 69 :* Nil)  $ \xs69 ->
    letAs' @(HList r) (MkT 68 :* xs69) $ \xs68 ->
    letAs' @(HList r) (MkT 67 :* xs68) $ \xs67 ->
    letAs' @(HList r) (MkT 66 :* xs67) $ \xs66 ->
    letAs' @(HList r) (MkT 65 :* xs66) $ \xs65 ->
    letAs' @(HList r) (MkT 64 :* xs65) $ \xs64 ->
    letAs' @(HList r) (MkT 63 :* xs64) $ \xs63 ->
    letAs' @(HList r) (MkT 62 :* xs63) $ \xs62 ->
    letAs' @(HList r) (MkT 61 :* xs62) $ \xs61 ->
    letAs' @(HList r) (MkT 60 :* xs61) $ \xs60 ->
    -- 59 .. 50
    letAs' @(HList r) (MkT 59 :* xs60) $ \xs59 ->
    letAs' @(HList r) (MkT 58 :* xs59) $ \xs58 ->
    letAs' @(HList r) (MkT 57 :* xs58) $ \xs57 ->
    letAs' @(HList r) (MkT 56 :* xs57) $ \xs56 ->
    letAs' @(HList r) (MkT 55 :* xs56) $ \xs55 ->
    letAs' @(HList r) (MkT 54 :* xs55) $ \xs54 ->
    letAs' @(HList r) (MkT 53 :* xs54) $ \xs53 ->
    letAs' @(HList r) (MkT 52 :* xs53) $ \xs52 ->
    letAs' @(HList r) (MkT 51 :* xs52) $ \xs51 ->
    letAs' @(HList r) (MkT 50 :* xs51) $ \xs50 ->
    -- 49 .. 40
    letAs' @(HList r) (MkT 49 :* xs50) $ \xs49 ->
    letAs' @(HList r) (MkT 48 :* xs49) $ \xs48 ->
    letAs' @(HList r) (MkT 47 :* xs48) $ \xs47 ->
    letAs' @(HList r) (MkT 46 :* xs47) $ \xs46 ->
    letAs' @(HList r) (MkT 45 :* xs46) $ \xs45 ->
    letAs' @(HList r) (MkT 44 :* xs45) $ \xs44 ->
    letAs' @(HList r) (MkT 43 :* xs44) $ \xs43 ->
    letAs' @(HList r) (MkT 42 :* xs43) $ \xs42 ->
    letAs' @(HList r) (MkT 41 :* xs42) $ \xs41 ->
    letAs' @(HList r) (MkT 40 :* xs41) $ \xs40 ->
    -- 39 .. 30
    letAs' @(HList r) (MkT 39 :* xs40) $ \xs39 ->
    letAs' @(HList r) (MkT 38 :* xs39) $ \xs38 ->
    letAs' @(HList r) (MkT 37 :* xs38) $ \xs37 ->
    letAs' @(HList r) (MkT 36 :* xs37) $ \xs36 ->
    letAs' @(HList r) (MkT 35 :* xs36) $ \xs35 ->
    letAs' @(HList r) (MkT 34 :* xs35) $ \xs34 ->
    letAs' @(HList r) (MkT 33 :* xs34) $ \xs33 ->
    letAs' @(HList r) (MkT 32 :* xs33) $ \xs32 ->
    letAs' @(HList r) (MkT 31 :* xs32) $ \xs31 ->
    letAs' @(HList r) (MkT 30 :* xs31) $ \xs30 ->
    -- 29 .. 20
    letAs' @(HList r) (MkT 29 :* xs30) $ \xs29 ->
    letAs' @(HList r) (MkT 28 :* xs29) $ \xs28 ->
    letAs' @(HList r) (MkT 27 :* xs28) $ \xs27 ->
    letAs' @(HList r) (MkT 26 :* xs27) $ \xs26 ->
    letAs' @(HList r) (MkT 25 :* xs26) $ \xs25 ->
    letAs' @(HList r) (MkT 24 :* xs25) $ \xs24 ->
    letAs' @(HList r) (MkT 23 :* xs24) $ \xs23 ->
    letAs' @(HList r) (MkT 22 :* xs23) $ \xs22 ->
    letAs' @(HList r) (MkT 21 :* xs22) $ \xs21 ->
    letAs' @(HList r) (MkT 20 :* xs21) $ \xs20 ->
    -- 19 .. 10
    letAs' @(HList r) (MkT 19 :* xs20) $ \xs19 ->
    letAs' @(HList r) (MkT 18 :* xs19) $ \xs18 ->
    letAs' @(HList r) (MkT 17 :* xs18) $ \xs17 ->
    letAs' @(HList r) (MkT 16 :* xs17) $ \xs16 ->
    letAs' @(HList r) (MkT 15 :* xs16) $ \xs15 ->
    letAs' @(HList r) (MkT 14 :* xs15) $ \xs14 ->
    letAs' @(HList r) (MkT 13 :* xs14) $ \xs13 ->
    letAs' @(HList r) (MkT 12 :* xs13) $ \xs12 ->
    letAs' @(HList r) (MkT 11 :* xs12) $ \xs11 ->
    letAs' @(HList r) (MkT 10 :* xs11) $ \xs10 ->
    -- 09 .. 00
    letAs' @(HList r) (MkT 09 :* xs10) $ \xs09 ->
    letAs' @(HList r) (MkT 08 :* xs09) $ \xs08 ->
    letAs' @(HList r) (MkT 07 :* xs08) $ \xs07 ->
    letAs' @(HList r) (MkT 06 :* xs07) $ \xs06 ->
    letAs' @(HList r) (MkT 05 :* xs06) $ \xs05 ->
    letAs' @(HList r) (MkT 04 :* xs05) $ \xs04 ->
    letAs' @(HList r) (MkT 03 :* xs04) $ \xs03 ->
    letAs' @(HList r) (MkT 02 :* xs03) $ \xs02 ->
    letAs' @(HList r) (MkT 01 :* xs02) $ \xs01 ->
    letAs' @(HList r) (MkT 00 :* xs01) $ \xs00 ->
      castEqual xs00
