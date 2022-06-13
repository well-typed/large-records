#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.HListLetAsCPS.Sized.R080 where

import TypeLet

import Bench.HList
import Bench.Types
import Common.HListOfSize.HL080

hlist :: HList Fields
hlist = letT' Proxy $ \(_ :: Proxy r) -> castEqual @(HList r) $
    -- 79 .. 70
    letAs' (MkT 79 :* Nil)  $ \xs79 ->
    letAs' (MkT 78 :* xs79) $ \xs78 ->
    letAs' (MkT 77 :* xs78) $ \xs77 ->
    letAs' (MkT 76 :* xs77) $ \xs76 ->
    letAs' (MkT 75 :* xs76) $ \xs75 ->
    letAs' (MkT 74 :* xs75) $ \xs74 ->
    letAs' (MkT 73 :* xs74) $ \xs73 ->
    letAs' (MkT 72 :* xs73) $ \xs72 ->
    letAs' (MkT 71 :* xs72) $ \xs71 ->
    letAs' (MkT 70 :* xs71) $ \xs70 ->
    -- 69 .. 60
    letAs' (MkT 69 :* xs70) $ \xs69 ->
    letAs' (MkT 68 :* xs69) $ \xs68 ->
    letAs' (MkT 67 :* xs68) $ \xs67 ->
    letAs' (MkT 66 :* xs67) $ \xs66 ->
    letAs' (MkT 65 :* xs66) $ \xs65 ->
    letAs' (MkT 64 :* xs65) $ \xs64 ->
    letAs' (MkT 63 :* xs64) $ \xs63 ->
    letAs' (MkT 62 :* xs63) $ \xs62 ->
    letAs' (MkT 61 :* xs62) $ \xs61 ->
    letAs' (MkT 60 :* xs61) $ \xs60 ->
    -- 59 .. 50
    letAs' (MkT 59 :* xs60) $ \xs59 ->
    letAs' (MkT 58 :* xs59) $ \xs58 ->
    letAs' (MkT 57 :* xs58) $ \xs57 ->
    letAs' (MkT 56 :* xs57) $ \xs56 ->
    letAs' (MkT 55 :* xs56) $ \xs55 ->
    letAs' (MkT 54 :* xs55) $ \xs54 ->
    letAs' (MkT 53 :* xs54) $ \xs53 ->
    letAs' (MkT 52 :* xs53) $ \xs52 ->
    letAs' (MkT 51 :* xs52) $ \xs51 ->
    letAs' (MkT 50 :* xs51) $ \xs50 ->
    -- 49 .. 40
    letAs' (MkT 49 :* xs50) $ \xs49 ->
    letAs' (MkT 48 :* xs49) $ \xs48 ->
    letAs' (MkT 47 :* xs48) $ \xs47 ->
    letAs' (MkT 46 :* xs47) $ \xs46 ->
    letAs' (MkT 45 :* xs46) $ \xs45 ->
    letAs' (MkT 44 :* xs45) $ \xs44 ->
    letAs' (MkT 43 :* xs44) $ \xs43 ->
    letAs' (MkT 42 :* xs43) $ \xs42 ->
    letAs' (MkT 41 :* xs42) $ \xs41 ->
    letAs' (MkT 40 :* xs41) $ \xs40 ->
    -- 39 .. 30
    letAs' (MkT 39 :* xs40) $ \xs39 ->
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
