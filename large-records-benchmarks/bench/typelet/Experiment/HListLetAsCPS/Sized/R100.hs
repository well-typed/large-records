#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.HListLetAsCPS.Sized.R100 where

import TypeLet

import Bench.HList
import Bench.Types
import Common.HListOfSize.HL100

hlist :: HList Fields
hlist = letT' (Proxy @Fields) $ \(_ :: Proxy r) -> castEqual $
    -- 99 .. 90
    letAs' @(HList r) (MkT 99 :* Nil)  $ \(xs99 :: HList t99) ->
    letAs' @(HList r) (MkT 98 :* xs99) $ \(xs98 :: HList t98) ->
    letAs' @(HList r) (MkT 97 :* xs98) $ \(xs97 :: HList t97) ->
    letAs' @(HList r) (MkT 96 :* xs97) $ \(xs96 :: HList t96) ->
    letAs' @(HList r) (MkT 95 :* xs96) $ \(xs95 :: HList t95) ->
    letAs' @(HList r) (MkT 94 :* xs95) $ \(xs94 :: HList t94) ->
    letAs' @(HList r) (MkT 93 :* xs94) $ \(xs93 :: HList t93) ->
    letAs' @(HList r) (MkT 92 :* xs93) $ \(xs92 :: HList t92) ->
    letAs' @(HList r) (MkT 91 :* xs92) $ \(xs91 :: HList t91) ->
    letAs' @(HList r) (MkT 90 :* xs91) $ \(xs90 :: HList t90) ->
    -- 89 .. 80
    letAs' @(HList r) (MkT 89 :* xs90) $ \(xs89 :: HList t89) ->
    letAs' @(HList r) (MkT 88 :* xs89) $ \(xs88 :: HList t88) ->
    letAs' @(HList r) (MkT 87 :* xs88) $ \(xs87 :: HList t87) ->
    letAs' @(HList r) (MkT 86 :* xs87) $ \(xs86 :: HList t86) ->
    letAs' @(HList r) (MkT 85 :* xs86) $ \(xs85 :: HList t85) ->
    letAs' @(HList r) (MkT 84 :* xs85) $ \(xs84 :: HList t84) ->
    letAs' @(HList r) (MkT 83 :* xs84) $ \(xs83 :: HList t83) ->
    letAs' @(HList r) (MkT 82 :* xs83) $ \(xs82 :: HList t82) ->
    letAs' @(HList r) (MkT 81 :* xs82) $ \(xs81 :: HList t81) ->
    letAs' @(HList r) (MkT 80 :* xs81) $ \(xs80 :: HList t80) ->
    -- 79 .. 70
    letAs' @(HList r) (MkT 79 :* xs80) $ \(xs79 :: HList t79) ->
    letAs' @(HList r) (MkT 78 :* xs79) $ \(xs78 :: HList t78) ->
    letAs' @(HList r) (MkT 77 :* xs78) $ \(xs77 :: HList t77) ->
    letAs' @(HList r) (MkT 76 :* xs77) $ \(xs76 :: HList t76) ->
    letAs' @(HList r) (MkT 75 :* xs76) $ \(xs75 :: HList t75) ->
    letAs' @(HList r) (MkT 74 :* xs75) $ \(xs74 :: HList t74) ->
    letAs' @(HList r) (MkT 73 :* xs74) $ \(xs73 :: HList t73) ->
    letAs' @(HList r) (MkT 72 :* xs73) $ \(xs72 :: HList t72) ->
    letAs' @(HList r) (MkT 71 :* xs72) $ \(xs71 :: HList t71) ->
    letAs' @(HList r) (MkT 70 :* xs71) $ \(xs70 :: HList t70) ->
    -- 69 .. 60
    letAs' @(HList r) (MkT 69 :* xs70) $ \(xs69 :: HList t69) ->
    letAs' @(HList r) (MkT 68 :* xs69) $ \(xs68 :: HList t68) ->
    letAs' @(HList r) (MkT 67 :* xs68) $ \(xs67 :: HList t67) ->
    letAs' @(HList r) (MkT 66 :* xs67) $ \(xs66 :: HList t66) ->
    letAs' @(HList r) (MkT 65 :* xs66) $ \(xs65 :: HList t65) ->
    letAs' @(HList r) (MkT 64 :* xs65) $ \(xs64 :: HList t64) ->
    letAs' @(HList r) (MkT 63 :* xs64) $ \(xs63 :: HList t63) ->
    letAs' @(HList r) (MkT 62 :* xs63) $ \(xs62 :: HList t62) ->
    letAs' @(HList r) (MkT 61 :* xs62) $ \(xs61 :: HList t61) ->
    letAs' @(HList r) (MkT 60 :* xs61) $ \(xs60 :: HList t60) ->
    -- 59 .. 50
    letAs' @(HList r) (MkT 59 :* xs60) $ \(xs59 :: HList t59) ->
    letAs' @(HList r) (MkT 58 :* xs59) $ \(xs58 :: HList t58) ->
    letAs' @(HList r) (MkT 57 :* xs58) $ \(xs57 :: HList t57) ->
    letAs' @(HList r) (MkT 56 :* xs57) $ \(xs56 :: HList t56) ->
    letAs' @(HList r) (MkT 55 :* xs56) $ \(xs55 :: HList t55) ->
    letAs' @(HList r) (MkT 54 :* xs55) $ \(xs54 :: HList t54) ->
    letAs' @(HList r) (MkT 53 :* xs54) $ \(xs53 :: HList t53) ->
    letAs' @(HList r) (MkT 52 :* xs53) $ \(xs52 :: HList t52) ->
    letAs' @(HList r) (MkT 51 :* xs52) $ \(xs51 :: HList t51) ->
    letAs' @(HList r) (MkT 50 :* xs51) $ \(xs50 :: HList t50) ->
    -- 49 .. 40
    letAs' @(HList r) (MkT 49 :* xs50) $ \(xs49 :: HList t49) ->
    letAs' @(HList r) (MkT 48 :* xs49) $ \(xs48 :: HList t48) ->
    letAs' @(HList r) (MkT 47 :* xs48) $ \(xs47 :: HList t47) ->
    letAs' @(HList r) (MkT 46 :* xs47) $ \(xs46 :: HList t46) ->
    letAs' @(HList r) (MkT 45 :* xs46) $ \(xs45 :: HList t45) ->
    letAs' @(HList r) (MkT 44 :* xs45) $ \(xs44 :: HList t44) ->
    letAs' @(HList r) (MkT 43 :* xs44) $ \(xs43 :: HList t43) ->
    letAs' @(HList r) (MkT 42 :* xs43) $ \(xs42 :: HList t42) ->
    letAs' @(HList r) (MkT 41 :* xs42) $ \(xs41 :: HList t41) ->
    letAs' @(HList r) (MkT 40 :* xs41) $ \(xs40 :: HList t40) ->
    -- 39 .. 30
    letAs' @(HList r) (MkT 39 :* xs40) $ \(xs39 :: HList t39) ->
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
