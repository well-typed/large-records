{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Test.Size.HList.LetAs.LetAs040 where

import TypeLet

import Test.Infra
import Test.Size.HList.Index.Ix040

hlist :: HList Fields
hlist =
    -- 39 .. 30
    case letAs (HCons (MkT @"i39") HNil) of { LetAs (xs39 :: HList t39) ->
    case letAs (HCons (MkT @"i38") xs39) of { LetAs (xs38 :: HList t38) ->
    case letAs (HCons (MkT @"i37") xs38) of { LetAs (xs37 :: HList t37) ->
    case letAs (HCons (MkT @"i36") xs37) of { LetAs (xs36 :: HList t36) ->
    case letAs (HCons (MkT @"i35") xs36) of { LetAs (xs35 :: HList t35) ->
    case letAs (HCons (MkT @"i34") xs35) of { LetAs (xs34 :: HList t34) ->
    case letAs (HCons (MkT @"i33") xs34) of { LetAs (xs33 :: HList t33) ->
    case letAs (HCons (MkT @"i32") xs33) of { LetAs (xs32 :: HList t32) ->
    case letAs (HCons (MkT @"i31") xs32) of { LetAs (xs31 :: HList t31) ->
    case letAs (HCons (MkT @"i30") xs31) of { LetAs (xs30 :: HList t30) ->
    -- 29 .. 20
    case letAs (HCons (MkT @"i29") xs30) of { LetAs (xs29 :: HList t29) ->
    case letAs (HCons (MkT @"i28") xs29) of { LetAs (xs28 :: HList t28) ->
    case letAs (HCons (MkT @"i27") xs28) of { LetAs (xs27 :: HList t27) ->
    case letAs (HCons (MkT @"i26") xs27) of { LetAs (xs26 :: HList t26) ->
    case letAs (HCons (MkT @"i25") xs26) of { LetAs (xs25 :: HList t25) ->
    case letAs (HCons (MkT @"i24") xs25) of { LetAs (xs24 :: HList t24) ->
    case letAs (HCons (MkT @"i23") xs24) of { LetAs (xs23 :: HList t23) ->
    case letAs (HCons (MkT @"i22") xs23) of { LetAs (xs22 :: HList t22) ->
    case letAs (HCons (MkT @"i21") xs22) of { LetAs (xs21 :: HList t21) ->
    case letAs (HCons (MkT @"i20") xs21) of { LetAs (xs20 :: HList t20) ->
    -- 19 .. 10
    case letAs (HCons (MkT @"i19") xs20) of { LetAs (xs19 :: HList t19) ->
    case letAs (HCons (MkT @"i18") xs19) of { LetAs (xs18 :: HList t18) ->
    case letAs (HCons (MkT @"i17") xs18) of { LetAs (xs17 :: HList t17) ->
    case letAs (HCons (MkT @"i16") xs17) of { LetAs (xs16 :: HList t16) ->
    case letAs (HCons (MkT @"i15") xs16) of { LetAs (xs15 :: HList t15) ->
    case letAs (HCons (MkT @"i14") xs15) of { LetAs (xs14 :: HList t14) ->
    case letAs (HCons (MkT @"i13") xs14) of { LetAs (xs13 :: HList t13) ->
    case letAs (HCons (MkT @"i12") xs13) of { LetAs (xs12 :: HList t12) ->
    case letAs (HCons (MkT @"i11") xs12) of { LetAs (xs11 :: HList t11) ->
    case letAs (HCons (MkT @"i10") xs11) of { LetAs (xs10 :: HList t10) ->
    -- 09 .. 00
    case letAs (HCons (MkT @"i09") xs10) of { LetAs (xs09 :: HList t09) ->
    case letAs (HCons (MkT @"i08") xs09) of { LetAs (xs08 :: HList t08) ->
    case letAs (HCons (MkT @"i07") xs08) of { LetAs (xs07 :: HList t07) ->
    case letAs (HCons (MkT @"i06") xs07) of { LetAs (xs06 :: HList t06) ->
    case letAs (HCons (MkT @"i05") xs06) of { LetAs (xs05 :: HList t05) ->
    case letAs (HCons (MkT @"i04") xs05) of { LetAs (xs04 :: HList t04) ->
    case letAs (HCons (MkT @"i03") xs04) of { LetAs (xs03 :: HList t03) ->
    case letAs (HCons (MkT @"i02") xs03) of { LetAs (xs02 :: HList t02) ->
    case letAs (HCons (MkT @"i01") xs02) of { LetAs (xs01 :: HList t01) ->
    case letAs (HCons (MkT @"i00") xs01) of { LetAs (xs00 :: HList t00) ->
      castEqual xs00
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
