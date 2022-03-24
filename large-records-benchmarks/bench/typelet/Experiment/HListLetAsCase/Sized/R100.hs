#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.HListLetAsCase.Sized.R100 where

import TypeLet

import Bench.HList
import Bench.Types
import Common.HListOfSize.HL100

hlist :: HList Fields
hlist =
    -- 99 .. 90
    case letAs (MkT 99 :* Nil)  of { LetAs (xs99 :: HList t99) ->
    case letAs (MkT 98 :* xs99) of { LetAs (xs98 :: HList t98) ->
    case letAs (MkT 97 :* xs98) of { LetAs (xs97 :: HList t97) ->
    case letAs (MkT 96 :* xs97) of { LetAs (xs96 :: HList t96) ->
    case letAs (MkT 95 :* xs96) of { LetAs (xs95 :: HList t95) ->
    case letAs (MkT 94 :* xs95) of { LetAs (xs94 :: HList t94) ->
    case letAs (MkT 93 :* xs94) of { LetAs (xs93 :: HList t93) ->
    case letAs (MkT 92 :* xs93) of { LetAs (xs92 :: HList t92) ->
    case letAs (MkT 91 :* xs92) of { LetAs (xs91 :: HList t91) ->
    case letAs (MkT 90 :* xs91) of { LetAs (xs90 :: HList t90) ->
    -- 89 .. 80
    case letAs (MkT 89 :* xs90) of { LetAs (xs89 :: HList t89) ->
    case letAs (MkT 88 :* xs89) of { LetAs (xs88 :: HList t88) ->
    case letAs (MkT 87 :* xs88) of { LetAs (xs87 :: HList t87) ->
    case letAs (MkT 86 :* xs87) of { LetAs (xs86 :: HList t86) ->
    case letAs (MkT 85 :* xs86) of { LetAs (xs85 :: HList t85) ->
    case letAs (MkT 84 :* xs85) of { LetAs (xs84 :: HList t84) ->
    case letAs (MkT 83 :* xs84) of { LetAs (xs83 :: HList t83) ->
    case letAs (MkT 82 :* xs83) of { LetAs (xs82 :: HList t82) ->
    case letAs (MkT 81 :* xs82) of { LetAs (xs81 :: HList t81) ->
    case letAs (MkT 80 :* xs81) of { LetAs (xs80 :: HList t80) ->
    -- 79 .. 70
    case letAs (MkT 79 :* xs80) of { LetAs (xs79 :: HList t79) ->
    case letAs (MkT 78 :* xs79) of { LetAs (xs78 :: HList t78) ->
    case letAs (MkT 77 :* xs78) of { LetAs (xs77 :: HList t77) ->
    case letAs (MkT 76 :* xs77) of { LetAs (xs76 :: HList t76) ->
    case letAs (MkT 75 :* xs76) of { LetAs (xs75 :: HList t75) ->
    case letAs (MkT 74 :* xs75) of { LetAs (xs74 :: HList t74) ->
    case letAs (MkT 73 :* xs74) of { LetAs (xs73 :: HList t73) ->
    case letAs (MkT 72 :* xs73) of { LetAs (xs72 :: HList t72) ->
    case letAs (MkT 71 :* xs72) of { LetAs (xs71 :: HList t71) ->
    case letAs (MkT 70 :* xs71) of { LetAs (xs70 :: HList t70) ->
    -- 69 .. 60
    case letAs (MkT 69 :* xs70) of { LetAs (xs69 :: HList t69) ->
    case letAs (MkT 68 :* xs69) of { LetAs (xs68 :: HList t68) ->
    case letAs (MkT 67 :* xs68) of { LetAs (xs67 :: HList t67) ->
    case letAs (MkT 66 :* xs67) of { LetAs (xs66 :: HList t66) ->
    case letAs (MkT 65 :* xs66) of { LetAs (xs65 :: HList t65) ->
    case letAs (MkT 64 :* xs65) of { LetAs (xs64 :: HList t64) ->
    case letAs (MkT 63 :* xs64) of { LetAs (xs63 :: HList t63) ->
    case letAs (MkT 62 :* xs63) of { LetAs (xs62 :: HList t62) ->
    case letAs (MkT 61 :* xs62) of { LetAs (xs61 :: HList t61) ->
    case letAs (MkT 60 :* xs61) of { LetAs (xs60 :: HList t60) ->
    -- 59 .. 50
    case letAs (MkT 59 :* xs60) of { LetAs (xs59 :: HList t59) ->
    case letAs (MkT 58 :* xs59) of { LetAs (xs58 :: HList t58) ->
    case letAs (MkT 57 :* xs58) of { LetAs (xs57 :: HList t57) ->
    case letAs (MkT 56 :* xs57) of { LetAs (xs56 :: HList t56) ->
    case letAs (MkT 55 :* xs56) of { LetAs (xs55 :: HList t55) ->
    case letAs (MkT 54 :* xs55) of { LetAs (xs54 :: HList t54) ->
    case letAs (MkT 53 :* xs54) of { LetAs (xs53 :: HList t53) ->
    case letAs (MkT 52 :* xs53) of { LetAs (xs52 :: HList t52) ->
    case letAs (MkT 51 :* xs52) of { LetAs (xs51 :: HList t51) ->
    case letAs (MkT 50 :* xs51) of { LetAs (xs50 :: HList t50) ->
    -- 49 .. 40
    case letAs (MkT 49 :* xs50) of { LetAs (xs49 :: HList t49) ->
    case letAs (MkT 48 :* xs49) of { LetAs (xs48 :: HList t48) ->
    case letAs (MkT 47 :* xs48) of { LetAs (xs47 :: HList t47) ->
    case letAs (MkT 46 :* xs47) of { LetAs (xs46 :: HList t46) ->
    case letAs (MkT 45 :* xs46) of { LetAs (xs45 :: HList t45) ->
    case letAs (MkT 44 :* xs45) of { LetAs (xs44 :: HList t44) ->
    case letAs (MkT 43 :* xs44) of { LetAs (xs43 :: HList t43) ->
    case letAs (MkT 42 :* xs43) of { LetAs (xs42 :: HList t42) ->
    case letAs (MkT 41 :* xs42) of { LetAs (xs41 :: HList t41) ->
    case letAs (MkT 40 :* xs41) of { LetAs (xs40 :: HList t40) ->
    -- 39 .. 30
    case letAs (MkT 39 :* xs40) of { LetAs (xs39 :: HList t39) ->
    case letAs (MkT 38 :* xs39) of { LetAs (xs38 :: HList t38) ->
    case letAs (MkT 37 :* xs38) of { LetAs (xs37 :: HList t37) ->
    case letAs (MkT 36 :* xs37) of { LetAs (xs36 :: HList t36) ->
    case letAs (MkT 35 :* xs36) of { LetAs (xs35 :: HList t35) ->
    case letAs (MkT 34 :* xs35) of { LetAs (xs34 :: HList t34) ->
    case letAs (MkT 33 :* xs34) of { LetAs (xs33 :: HList t33) ->
    case letAs (MkT 32 :* xs33) of { LetAs (xs32 :: HList t32) ->
    case letAs (MkT 31 :* xs32) of { LetAs (xs31 :: HList t31) ->
    case letAs (MkT 30 :* xs31) of { LetAs (xs30 :: HList t30) ->
    -- 29 .. 20
    case letAs (MkT 29 :* xs30) of { LetAs (xs29 :: HList t29) ->
    case letAs (MkT 28 :* xs29) of { LetAs (xs28 :: HList t28) ->
    case letAs (MkT 27 :* xs28) of { LetAs (xs27 :: HList t27) ->
    case letAs (MkT 26 :* xs27) of { LetAs (xs26 :: HList t26) ->
    case letAs (MkT 25 :* xs26) of { LetAs (xs25 :: HList t25) ->
    case letAs (MkT 24 :* xs25) of { LetAs (xs24 :: HList t24) ->
    case letAs (MkT 23 :* xs24) of { LetAs (xs23 :: HList t23) ->
    case letAs (MkT 22 :* xs23) of { LetAs (xs22 :: HList t22) ->
    case letAs (MkT 21 :* xs22) of { LetAs (xs21 :: HList t21) ->
    case letAs (MkT 20 :* xs21) of { LetAs (xs20 :: HList t20) ->
    -- 19 .. 10
    case letAs (MkT 19 :* xs20) of { LetAs (xs19 :: HList t19) ->
    case letAs (MkT 18 :* xs19) of { LetAs (xs18 :: HList t18) ->
    case letAs (MkT 17 :* xs18) of { LetAs (xs17 :: HList t17) ->
    case letAs (MkT 16 :* xs17) of { LetAs (xs16 :: HList t16) ->
    case letAs (MkT 15 :* xs16) of { LetAs (xs15 :: HList t15) ->
    case letAs (MkT 14 :* xs15) of { LetAs (xs14 :: HList t14) ->
    case letAs (MkT 13 :* xs14) of { LetAs (xs13 :: HList t13) ->
    case letAs (MkT 12 :* xs13) of { LetAs (xs12 :: HList t12) ->
    case letAs (MkT 11 :* xs12) of { LetAs (xs11 :: HList t11) ->
    case letAs (MkT 10 :* xs11) of { LetAs (xs10 :: HList t10) ->
    -- 09 .. 00
    case letAs (MkT 09 :* xs10) of { LetAs (xs09 :: HList t09) ->
    case letAs (MkT 08 :* xs09) of { LetAs (xs08 :: HList t08) ->
    case letAs (MkT 07 :* xs08) of { LetAs (xs07 :: HList t07) ->
    case letAs (MkT 06 :* xs07) of { LetAs (xs06 :: HList t06) ->
    case letAs (MkT 05 :* xs06) of { LetAs (xs05 :: HList t05) ->
    case letAs (MkT 04 :* xs05) of { LetAs (xs04 :: HList t04) ->
    case letAs (MkT 03 :* xs04) of { LetAs (xs03 :: HList t03) ->
    case letAs (MkT 02 :* xs03) of { LetAs (xs02 :: HList t02) ->
    case letAs (MkT 01 :* xs02) of { LetAs (xs01 :: HList t01) ->
    case letAs (MkT 00 :* xs01) of { LetAs (xs00 :: HList t00) ->
      castEqual xs00
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
