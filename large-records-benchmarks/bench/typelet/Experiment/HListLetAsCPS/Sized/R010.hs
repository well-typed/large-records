#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.HListLetAsCPS.Sized.R010 where

import TypeLet

import Bench.HList
import Bench.Types
import Common.HListOfSize.HL010

hlist :: HList Fields
hlist = letT' Proxy $ \(_ :: Proxy r) -> castEqual @(HList r) $
    -- 09 .. 00
    letAs' (MkT 09 :* Nil)  $ \xs09 ->
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
