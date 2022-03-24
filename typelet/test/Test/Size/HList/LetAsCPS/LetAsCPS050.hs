{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Test.Size.HList.LetAsCPS.LetAsCPS050 where

import TypeLet

import Test.Infra
import Test.Size.HList.Index.Ix050

hlist :: HList Fields
hlist = letT' (Proxy @Fields) $ \(_ :: Proxy r) -> castEqual $
    -- 49 .. 40
    letAs' @(HList r) (HCons (MkT @"i49") HNil) $ \(xs49 :: HList t49) ->
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
