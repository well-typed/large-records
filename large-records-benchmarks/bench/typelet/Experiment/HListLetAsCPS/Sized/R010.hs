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
hlist = letT' Proxy $ \(_ :: Proxy r) -> castEqual $
    -- 09 .. 00
    letAs' @(HList r) (MkT 09 :* Nil)  $ \xs09 ->
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
