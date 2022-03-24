#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.HListLetAsCPS.Sized.R020 where

import TypeLet

import Bench.HList
import Bench.Types
import Common.HListOfSize.HL020

hlist :: HList Fields
hlist = letT' Proxy $ \(_ :: Proxy r) -> castEqual $
    -- 19 .. 10
    letAs' @(HList r) (MkT 19 :* Nil)  $ \xs19 ->
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
