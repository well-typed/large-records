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
hlist = letT' (Proxy @Fields) $ \(_ :: Proxy r) -> castEqual $
    -- 39 .. 30
    letAs' @(HList r) (MkT 39 :* Nil)  $ \(xs39 :: HList t39) ->
    letAs' @(HList r) (MkT 38 :* xs39) $ \(xs38 :: HList t38) ->
    letAs' @(HList r) (MkT 37 :* xs38) $ \(xs37 :: HList t37) ->
    letAs' @(HList r) (MkT 36 :* xs37) $ \(xs36 :: HList t36) ->
    letAs' @(HList r) (MkT 35 :* xs36) $ \(xs35 :: HList t35) ->
    letAs' @(HList r) (MkT 34 :* xs35) $ \(xs34 :: HList t34) ->
    letAs' @(HList r) (MkT 33 :* xs34) $ \(xs33 :: HList t33) ->
    letAs' @(HList r) (MkT 32 :* xs33) $ \(xs32 :: HList t32) ->
    letAs' @(HList r) (MkT 31 :* xs32) $ \(xs31 :: HList t31) ->
    letAs' @(HList r) (MkT 30 :* xs31) $ \(xs30 :: HList t30) ->
    -- 29 .. 20
    letAs' @(HList r) (MkT 29 :* xs30) $ \(xs29 :: HList t29) ->
    letAs' @(HList r) (MkT 28 :* xs29) $ \(xs28 :: HList t28) ->
    letAs' @(HList r) (MkT 27 :* xs28) $ \(xs27 :: HList t27) ->
    letAs' @(HList r) (MkT 26 :* xs27) $ \(xs26 :: HList t26) ->
    letAs' @(HList r) (MkT 25 :* xs26) $ \(xs25 :: HList t25) ->
    letAs' @(HList r) (MkT 24 :* xs25) $ \(xs24 :: HList t24) ->
    letAs' @(HList r) (MkT 23 :* xs24) $ \(xs23 :: HList t23) ->
    letAs' @(HList r) (MkT 22 :* xs23) $ \(xs22 :: HList t22) ->
    letAs' @(HList r) (MkT 21 :* xs22) $ \(xs21 :: HList t21) ->
    letAs' @(HList r) (MkT 20 :* xs21) $ \(xs20 :: HList t20) ->
    -- 19 .. 10
    letAs' @(HList r) (MkT 19 :* xs20) $ \(xs19 :: HList t19) ->
    letAs' @(HList r) (MkT 18 :* xs19) $ \(xs18 :: HList t18) ->
    letAs' @(HList r) (MkT 17 :* xs18) $ \(xs17 :: HList t17) ->
    letAs' @(HList r) (MkT 16 :* xs17) $ \(xs16 :: HList t16) ->
    letAs' @(HList r) (MkT 15 :* xs16) $ \(xs15 :: HList t15) ->
    letAs' @(HList r) (MkT 14 :* xs15) $ \(xs14 :: HList t14) ->
    letAs' @(HList r) (MkT 13 :* xs14) $ \(xs13 :: HList t13) ->
    letAs' @(HList r) (MkT 12 :* xs13) $ \(xs12 :: HList t12) ->
    letAs' @(HList r) (MkT 11 :* xs12) $ \(xs11 :: HList t11) ->
    letAs' @(HList r) (MkT 10 :* xs11) $ \(xs10 :: HList t10) ->
    -- 09 .. 00
    letAs' @(HList r) (MkT 09 :* xs10) $ \(xs09 :: HList t09) ->
    letAs' @(HList r) (MkT 08 :* xs09) $ \(xs08 :: HList t08) ->
    letAs' @(HList r) (MkT 07 :* xs08) $ \(xs07 :: HList t07) ->
    letAs' @(HList r) (MkT 06 :* xs07) $ \(xs06 :: HList t06) ->
    letAs' @(HList r) (MkT 05 :* xs06) $ \(xs05 :: HList t05) ->
    letAs' @(HList r) (MkT 04 :* xs05) $ \(xs04 :: HList t04) ->
    letAs' @(HList r) (MkT 03 :* xs04) $ \(xs03 :: HList t03) ->
    letAs' @(HList r) (MkT 02 :* xs03) $ \(xs02 :: HList t02) ->
    letAs' @(HList r) (MkT 01 :* xs02) $ \(xs01 :: HList t01) ->
    letAs' @(HList r) (MkT 00 :* xs01) $ \(xs00 :: HList t00) ->
      castEqual xs00
