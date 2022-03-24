{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Test.Size.HList.LetAsCPS.LetAsCPS090 where

import TypeLet

import Test.Infra
import Test.Size.HList.Index.Ix090

hlist :: HList Fields
hlist = letT' (Proxy @Fields) $ \(_ :: Proxy r) -> castEqual $
    -- 89 .. 80
    letAs' @(HList r) (HCons (MkT @"i89") HNil) $ \(xs89 :: HList t89) ->
    letAs' @(HList r) (HCons (MkT @"i88") xs89) $ \(xs88 :: HList t88) ->
    letAs' @(HList r) (HCons (MkT @"i87") xs88) $ \(xs87 :: HList t87) ->
    letAs' @(HList r) (HCons (MkT @"i86") xs87) $ \(xs86 :: HList t86) ->
    letAs' @(HList r) (HCons (MkT @"i85") xs86) $ \(xs85 :: HList t85) ->
    letAs' @(HList r) (HCons (MkT @"i84") xs85) $ \(xs84 :: HList t84) ->
    letAs' @(HList r) (HCons (MkT @"i83") xs84) $ \(xs83 :: HList t83) ->
    letAs' @(HList r) (HCons (MkT @"i82") xs83) $ \(xs82 :: HList t82) ->
    letAs' @(HList r) (HCons (MkT @"i81") xs82) $ \(xs81 :: HList t81) ->
    letAs' @(HList r) (HCons (MkT @"i80") xs81) $ \(xs80 :: HList t80) ->
    -- 79 .. 70
    letAs' @(HList r) (HCons (MkT @"i79") xs80) $ \(xs79 :: HList t79) ->
    letAs' @(HList r) (HCons (MkT @"i78") xs79) $ \(xs78 :: HList t78) ->
    letAs' @(HList r) (HCons (MkT @"i77") xs78) $ \(xs77 :: HList t77) ->
    letAs' @(HList r) (HCons (MkT @"i76") xs77) $ \(xs76 :: HList t76) ->
    letAs' @(HList r) (HCons (MkT @"i75") xs76) $ \(xs75 :: HList t75) ->
    letAs' @(HList r) (HCons (MkT @"i74") xs75) $ \(xs74 :: HList t74) ->
    letAs' @(HList r) (HCons (MkT @"i73") xs74) $ \(xs73 :: HList t73) ->
    letAs' @(HList r) (HCons (MkT @"i72") xs73) $ \(xs72 :: HList t72) ->
    letAs' @(HList r) (HCons (MkT @"i71") xs72) $ \(xs71 :: HList t71) ->
    letAs' @(HList r) (HCons (MkT @"i70") xs71) $ \(xs70 :: HList t70) ->
    -- 69 .. 60
    letAs' @(HList r) (HCons (MkT @"i69") xs70) $ \(xs69 :: HList t69) ->
    letAs' @(HList r) (HCons (MkT @"i68") xs69) $ \(xs68 :: HList t68) ->
    letAs' @(HList r) (HCons (MkT @"i67") xs68) $ \(xs67 :: HList t67) ->
    letAs' @(HList r) (HCons (MkT @"i66") xs67) $ \(xs66 :: HList t66) ->
    letAs' @(HList r) (HCons (MkT @"i65") xs66) $ \(xs65 :: HList t65) ->
    letAs' @(HList r) (HCons (MkT @"i64") xs65) $ \(xs64 :: HList t64) ->
    letAs' @(HList r) (HCons (MkT @"i63") xs64) $ \(xs63 :: HList t63) ->
    letAs' @(HList r) (HCons (MkT @"i62") xs63) $ \(xs62 :: HList t62) ->
    letAs' @(HList r) (HCons (MkT @"i61") xs62) $ \(xs61 :: HList t61) ->
    letAs' @(HList r) (HCons (MkT @"i60") xs61) $ \(xs60 :: HList t60) ->
    -- 59 .. 50
    letAs' @(HList r) (HCons (MkT @"i59") xs60) $ \(xs59 :: HList t59) ->
    letAs' @(HList r) (HCons (MkT @"i58") xs59) $ \(xs58 :: HList t58) ->
    letAs' @(HList r) (HCons (MkT @"i57") xs58) $ \(xs57 :: HList t57) ->
    letAs' @(HList r) (HCons (MkT @"i56") xs57) $ \(xs56 :: HList t56) ->
    letAs' @(HList r) (HCons (MkT @"i55") xs56) $ \(xs55 :: HList t55) ->
    letAs' @(HList r) (HCons (MkT @"i54") xs55) $ \(xs54 :: HList t54) ->
    letAs' @(HList r) (HCons (MkT @"i53") xs54) $ \(xs53 :: HList t53) ->
    letAs' @(HList r) (HCons (MkT @"i52") xs53) $ \(xs52 :: HList t52) ->
    letAs' @(HList r) (HCons (MkT @"i51") xs52) $ \(xs51 :: HList t51) ->
    letAs' @(HList r) (HCons (MkT @"i50") xs51) $ \(xs50 :: HList t50) ->
    -- 49 .. 40
    letAs' @(HList r) (HCons (MkT @"i49") xs50) $ \(xs49 :: HList t49) ->
    letAs' @(HList r) (HCons (MkT @"i48") xs49) $ \(xs48 :: HList t48) ->
    letAs' @(HList r) (HCons (MkT @"i47") xs48) $ \(xs47 :: HList t47) ->
    letAs' @(HList r) (HCons (MkT @"i46") xs47) $ \(xs46 :: HList t46) ->
    letAs' @(HList r) (HCons (MkT @"i45") xs46) $ \(xs45 :: HList t45) ->
    letAs' @(HList r) (HCons (MkT @"i44") xs45) $ \(xs44 :: HList t44) ->
    letAs' @(HList r) (HCons (MkT @"i43") xs44) $ \(xs43 :: HList t43) ->
    letAs' @(HList r) (HCons (MkT @"i42") xs43) $ \(xs42 :: HList t42) ->
    letAs' @(HList r) (HCons (MkT @"i41") xs42) $ \(xs41 :: HList t41) ->
    letAs' @(HList r) (HCons (MkT @"i40") xs41) $ \(xs40 :: HList t40) ->
    -- 39 .. 30
    letAs' @(HList r) (HCons (MkT @"i39") xs40) $ \(xs39 :: HList t39) ->
    letAs' @(HList r) (HCons (MkT @"i38") xs39) $ \(xs38 :: HList t38) ->
    letAs' @(HList r) (HCons (MkT @"i37") xs38) $ \(xs37 :: HList t37) ->
    letAs' @(HList r) (HCons (MkT @"i36") xs37) $ \(xs36 :: HList t36) ->
    letAs' @(HList r) (HCons (MkT @"i35") xs36) $ \(xs35 :: HList t35) ->
    letAs' @(HList r) (HCons (MkT @"i34") xs35) $ \(xs34 :: HList t34) ->
    letAs' @(HList r) (HCons (MkT @"i33") xs34) $ \(xs33 :: HList t33) ->
    letAs' @(HList r) (HCons (MkT @"i32") xs33) $ \(xs32 :: HList t32) ->
    letAs' @(HList r) (HCons (MkT @"i31") xs32) $ \(xs31 :: HList t31) ->
    letAs' @(HList r) (HCons (MkT @"i30") xs31) $ \(xs30 :: HList t30) ->
    -- 29 .. 20
    letAs' @(HList r) (HCons (MkT @"i29") xs30) $ \(xs29 :: HList t29) ->
    letAs' @(HList r) (HCons (MkT @"i28") xs29) $ \(xs28 :: HList t28) ->
    letAs' @(HList r) (HCons (MkT @"i27") xs28) $ \(xs27 :: HList t27) ->
    letAs' @(HList r) (HCons (MkT @"i26") xs27) $ \(xs26 :: HList t26) ->
    letAs' @(HList r) (HCons (MkT @"i25") xs26) $ \(xs25 :: HList t25) ->
    letAs' @(HList r) (HCons (MkT @"i24") xs25) $ \(xs24 :: HList t24) ->
    letAs' @(HList r) (HCons (MkT @"i23") xs24) $ \(xs23 :: HList t23) ->
    letAs' @(HList r) (HCons (MkT @"i22") xs23) $ \(xs22 :: HList t22) ->
    letAs' @(HList r) (HCons (MkT @"i21") xs22) $ \(xs21 :: HList t21) ->
    letAs' @(HList r) (HCons (MkT @"i20") xs21) $ \(xs20 :: HList t20) ->
    -- 19 .. 10
    letAs' @(HList r) (HCons (MkT @"i19") xs20) $ \(xs19 :: HList t19) ->
    letAs' @(HList r) (HCons (MkT @"i18") xs19) $ \(xs18 :: HList t18) ->
    letAs' @(HList r) (HCons (MkT @"i17") xs18) $ \(xs17 :: HList t17) ->
    letAs' @(HList r) (HCons (MkT @"i16") xs17) $ \(xs16 :: HList t16) ->
    letAs' @(HList r) (HCons (MkT @"i15") xs16) $ \(xs15 :: HList t15) ->
    letAs' @(HList r) (HCons (MkT @"i14") xs15) $ \(xs14 :: HList t14) ->
    letAs' @(HList r) (HCons (MkT @"i13") xs14) $ \(xs13 :: HList t13) ->
    letAs' @(HList r) (HCons (MkT @"i12") xs13) $ \(xs12 :: HList t12) ->
    letAs' @(HList r) (HCons (MkT @"i11") xs12) $ \(xs11 :: HList t11) ->
    letAs' @(HList r) (HCons (MkT @"i10") xs11) $ \(xs10 :: HList t10) ->
    -- 09 .. 00
    letAs' @(HList r) (HCons (MkT @"i09") xs10) $ \(xs09 :: HList t09) ->
    letAs' @(HList r) (HCons (MkT @"i08") xs09) $ \(xs08 :: HList t08) ->
    letAs' @(HList r) (HCons (MkT @"i07") xs08) $ \(xs07 :: HList t07) ->
    letAs' @(HList r) (HCons (MkT @"i06") xs07) $ \(xs06 :: HList t06) ->
    letAs' @(HList r) (HCons (MkT @"i05") xs06) $ \(xs05 :: HList t05) ->
    letAs' @(HList r) (HCons (MkT @"i04") xs05) $ \(xs04 :: HList t04) ->
    letAs' @(HList r) (HCons (MkT @"i03") xs04) $ \(xs03 :: HList t03) ->
    letAs' @(HList r) (HCons (MkT @"i02") xs03) $ \(xs02 :: HList t02) ->
    letAs' @(HList r) (HCons (MkT @"i01") xs02) $ \(xs01 :: HList t01) ->
    letAs' @(HList r) (HCons (MkT @"i00") xs01) $ \(xs00 :: HList t00) ->
      castEqual xs00
