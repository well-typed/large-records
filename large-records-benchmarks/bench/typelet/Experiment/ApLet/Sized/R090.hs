#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.ApLet.Sized.R090 where

import TypeLet

import Bench.Types
import Common.FunOfArity.F090

applyF :: forall f r. Applicative f => F r -> f r
applyF f =
    -- 89 .. 80
    case letT (Proxy @(T 89 -> r))   of { LetT (_ :: Proxy l89) ->
    case letT (Proxy @(T 88 -> l89)) of { LetT (_ :: Proxy l88) ->
    case letT (Proxy @(T 87 -> l88)) of { LetT (_ :: Proxy l87) ->
    case letT (Proxy @(T 86 -> l87)) of { LetT (_ :: Proxy l86) ->
    case letT (Proxy @(T 85 -> l86)) of { LetT (_ :: Proxy l85) ->
    case letT (Proxy @(T 84 -> l85)) of { LetT (_ :: Proxy l84) ->
    case letT (Proxy @(T 83 -> l84)) of { LetT (_ :: Proxy l83) ->
    case letT (Proxy @(T 82 -> l83)) of { LetT (_ :: Proxy l82) ->
    case letT (Proxy @(T 81 -> l82)) of { LetT (_ :: Proxy l81) ->
    case letT (Proxy @(T 80 -> l81)) of { LetT (_ :: Proxy l80) ->
    -- 79 .. 70
    case letT (Proxy @(T 79 -> l80)) of { LetT (_ :: Proxy l79) ->
    case letT (Proxy @(T 78 -> l79)) of { LetT (_ :: Proxy l78) ->
    case letT (Proxy @(T 77 -> l78)) of { LetT (_ :: Proxy l77) ->
    case letT (Proxy @(T 76 -> l77)) of { LetT (_ :: Proxy l76) ->
    case letT (Proxy @(T 75 -> l76)) of { LetT (_ :: Proxy l75) ->
    case letT (Proxy @(T 74 -> l75)) of { LetT (_ :: Proxy l74) ->
    case letT (Proxy @(T 73 -> l74)) of { LetT (_ :: Proxy l73) ->
    case letT (Proxy @(T 72 -> l73)) of { LetT (_ :: Proxy l72) ->
    case letT (Proxy @(T 71 -> l72)) of { LetT (_ :: Proxy l71) ->
    case letT (Proxy @(T 70 -> l71)) of { LetT (_ :: Proxy l70) ->
    -- 69 .. 60
    case letT (Proxy @(T 69 -> l70)) of { LetT (_ :: Proxy l69) ->
    case letT (Proxy @(T 68 -> l69)) of { LetT (_ :: Proxy l68) ->
    case letT (Proxy @(T 67 -> l68)) of { LetT (_ :: Proxy l67) ->
    case letT (Proxy @(T 66 -> l67)) of { LetT (_ :: Proxy l66) ->
    case letT (Proxy @(T 65 -> l66)) of { LetT (_ :: Proxy l65) ->
    case letT (Proxy @(T 64 -> l65)) of { LetT (_ :: Proxy l64) ->
    case letT (Proxy @(T 63 -> l64)) of { LetT (_ :: Proxy l63) ->
    case letT (Proxy @(T 62 -> l63)) of { LetT (_ :: Proxy l62) ->
    case letT (Proxy @(T 61 -> l62)) of { LetT (_ :: Proxy l61) ->
    case letT (Proxy @(T 60 -> l61)) of { LetT (_ :: Proxy l60) ->
    -- 59 .. 50
    case letT (Proxy @(T 59 -> l60)) of { LetT (_ :: Proxy l59) ->
    case letT (Proxy @(T 58 -> l59)) of { LetT (_ :: Proxy l58) ->
    case letT (Proxy @(T 57 -> l58)) of { LetT (_ :: Proxy l57) ->
    case letT (Proxy @(T 56 -> l57)) of { LetT (_ :: Proxy l56) ->
    case letT (Proxy @(T 55 -> l56)) of { LetT (_ :: Proxy l55) ->
    case letT (Proxy @(T 54 -> l55)) of { LetT (_ :: Proxy l54) ->
    case letT (Proxy @(T 53 -> l54)) of { LetT (_ :: Proxy l53) ->
    case letT (Proxy @(T 52 -> l53)) of { LetT (_ :: Proxy l52) ->
    case letT (Proxy @(T 51 -> l52)) of { LetT (_ :: Proxy l51) ->
    case letT (Proxy @(T 50 -> l51)) of { LetT (_ :: Proxy l50) ->
    -- 49 .. 40
    case letT (Proxy @(T 49 -> l50)) of { LetT (_ :: Proxy l49) ->
    case letT (Proxy @(T 48 -> l49)) of { LetT (_ :: Proxy l48) ->
    case letT (Proxy @(T 47 -> l48)) of { LetT (_ :: Proxy l47) ->
    case letT (Proxy @(T 46 -> l47)) of { LetT (_ :: Proxy l46) ->
    case letT (Proxy @(T 45 -> l46)) of { LetT (_ :: Proxy l45) ->
    case letT (Proxy @(T 44 -> l45)) of { LetT (_ :: Proxy l44) ->
    case letT (Proxy @(T 43 -> l44)) of { LetT (_ :: Proxy l43) ->
    case letT (Proxy @(T 42 -> l43)) of { LetT (_ :: Proxy l42) ->
    case letT (Proxy @(T 41 -> l42)) of { LetT (_ :: Proxy l41) ->
    case letT (Proxy @(T 40 -> l41)) of { LetT (_ :: Proxy l40) ->
    -- 39 .. 30
    case letT (Proxy @(T 39 -> l40)) of { LetT (_ :: Proxy l39) ->
    case letT (Proxy @(T 38 -> l39)) of { LetT (_ :: Proxy l38) ->
    case letT (Proxy @(T 37 -> l38)) of { LetT (_ :: Proxy l37) ->
    case letT (Proxy @(T 36 -> l37)) of { LetT (_ :: Proxy l36) ->
    case letT (Proxy @(T 35 -> l36)) of { LetT (_ :: Proxy l35) ->
    case letT (Proxy @(T 34 -> l35)) of { LetT (_ :: Proxy l34) ->
    case letT (Proxy @(T 33 -> l34)) of { LetT (_ :: Proxy l33) ->
    case letT (Proxy @(T 32 -> l33)) of { LetT (_ :: Proxy l32) ->
    case letT (Proxy @(T 31 -> l32)) of { LetT (_ :: Proxy l31) ->
    case letT (Proxy @(T 30 -> l31)) of { LetT (_ :: Proxy l30) ->
    -- 29 .. 20
    case letT (Proxy @(T 29 -> l30)) of { LetT (_ :: Proxy l29) ->
    case letT (Proxy @(T 28 -> l29)) of { LetT (_ :: Proxy l28) ->
    case letT (Proxy @(T 27 -> l28)) of { LetT (_ :: Proxy l27) ->
    case letT (Proxy @(T 26 -> l27)) of { LetT (_ :: Proxy l26) ->
    case letT (Proxy @(T 25 -> l26)) of { LetT (_ :: Proxy l25) ->
    case letT (Proxy @(T 24 -> l25)) of { LetT (_ :: Proxy l24) ->
    case letT (Proxy @(T 23 -> l24)) of { LetT (_ :: Proxy l23) ->
    case letT (Proxy @(T 22 -> l23)) of { LetT (_ :: Proxy l22) ->
    case letT (Proxy @(T 21 -> l22)) of { LetT (_ :: Proxy l21) ->
    case letT (Proxy @(T 20 -> l21)) of { LetT (_ :: Proxy l20) ->
    -- 19 .. 10
    case letT (Proxy @(T 19 -> l20)) of { LetT (_ :: Proxy l19) ->
    case letT (Proxy @(T 18 -> l19)) of { LetT (_ :: Proxy l18) ->
    case letT (Proxy @(T 17 -> l18)) of { LetT (_ :: Proxy l17) ->
    case letT (Proxy @(T 16 -> l17)) of { LetT (_ :: Proxy l16) ->
    case letT (Proxy @(T 15 -> l16)) of { LetT (_ :: Proxy l15) ->
    case letT (Proxy @(T 14 -> l15)) of { LetT (_ :: Proxy l14) ->
    case letT (Proxy @(T 13 -> l14)) of { LetT (_ :: Proxy l13) ->
    case letT (Proxy @(T 12 -> l13)) of { LetT (_ :: Proxy l12) ->
    case letT (Proxy @(T 11 -> l12)) of { LetT (_ :: Proxy l11) ->
    case letT (Proxy @(T 10 -> l11)) of { LetT (_ :: Proxy l10) ->
    -- 09 .. 00
    case letT (Proxy @(T 09 -> l10)) of { LetT (_ :: Proxy l09) ->
    case letT (Proxy @(T 08 -> l09)) of { LetT (_ :: Proxy l08) ->
    case letT (Proxy @(T 07 -> l08)) of { LetT (_ :: Proxy l07) ->
    case letT (Proxy @(T 06 -> l07)) of { LetT (_ :: Proxy l06) ->
    case letT (Proxy @(T 05 -> l06)) of { LetT (_ :: Proxy l05) ->
    case letT (Proxy @(T 04 -> l05)) of { LetT (_ :: Proxy l04) ->
    case letT (Proxy @(T 03 -> l04)) of { LetT (_ :: Proxy l03) ->
    case letT (Proxy @(T 02 -> l03)) of { LetT (_ :: Proxy l02) ->
    case letT (Proxy @(T 01 -> l02)) of { LetT (_ :: Proxy l01) ->
    case letT (Proxy @(T 00 -> l01)) of { LetT (_ :: Proxy l00) ->

      let -- 00 .. 09
          f00 :: f l00
          f01 :: f l01
          f02 :: f l02
          f03 :: f l03
          f04 :: f l04
          f05 :: f l05
          f06 :: f l06
          f07 :: f l07
          f08 :: f l08
          f09 :: f l09
          -- 10 .. 19
          f10 :: f l10
          f11 :: f l11
          f12 :: f l12
          f13 :: f l13
          f14 :: f l14
          f15 :: f l15
          f16 :: f l16
          f17 :: f l17
          f18 :: f l18
          f19 :: f l19
          -- 20 .. 29
          f20 :: f l20
          f21 :: f l21
          f22 :: f l22
          f23 :: f l23
          f24 :: f l24
          f25 :: f l25
          f26 :: f l26
          f27 :: f l27
          f28 :: f l28
          f29 :: f l29
          -- 30 .. 39
          f30 :: f l30
          f31 :: f l31
          f32 :: f l32
          f33 :: f l33
          f34 :: f l34
          f35 :: f l35
          f36 :: f l36
          f37 :: f l37
          f38 :: f l38
          f39 :: f l39
          -- 40 .. 49
          f40 :: f l40
          f41 :: f l41
          f42 :: f l42
          f43 :: f l43
          f44 :: f l44
          f45 :: f l45
          f46 :: f l46
          f47 :: f l47
          f48 :: f l48
          f49 :: f l49
          -- 50 .. 59
          f50 :: f l50
          f51 :: f l51
          f52 :: f l52
          f53 :: f l53
          f54 :: f l54
          f55 :: f l55
          f56 :: f l56
          f57 :: f l57
          f58 :: f l58
          f59 :: f l59
          -- 60 .. 69
          f60 :: f l60
          f61 :: f l61
          f62 :: f l62
          f63 :: f l63
          f64 :: f l64
          f65 :: f l65
          f66 :: f l66
          f67 :: f l67
          f68 :: f l68
          f69 :: f l69
          -- 70 .. 79
          f70 :: f l70
          f71 :: f l71
          f72 :: f l72
          f73 :: f l73
          f74 :: f l74
          f75 :: f l75
          f76 :: f l76
          f77 :: f l77
          f78 :: f l78
          f79 :: f l79
          -- 80 .. 89
          f80 :: f l80
          f81 :: f l81
          f82 :: f l82
          f83 :: f l83
          f84 :: f l84
          f85 :: f l85
          f86 :: f l86
          f87 :: f l87
          f88 :: f l88
          f89 :: f l89

          res :: f r

          -- 00 .. 09
          f00 = pure (castEqual f)
          f01 = castEqual f00 <*> pure (MkT 00)
          f02 = castEqual f01 <*> pure (MkT 01)
          f03 = castEqual f02 <*> pure (MkT 02)
          f04 = castEqual f03 <*> pure (MkT 03)
          f05 = castEqual f04 <*> pure (MkT 04)
          f06 = castEqual f05 <*> pure (MkT 05)
          f07 = castEqual f06 <*> pure (MkT 06)
          f08 = castEqual f07 <*> pure (MkT 07)
          f09 = castEqual f08 <*> pure (MkT 08)
          -- 10 .. 19
          f10 = castEqual f09 <*> pure (MkT 09)
          f11 = castEqual f10 <*> pure (MkT 10)
          f12 = castEqual f11 <*> pure (MkT 11)
          f13 = castEqual f12 <*> pure (MkT 12)
          f14 = castEqual f13 <*> pure (MkT 13)
          f15 = castEqual f14 <*> pure (MkT 14)
          f16 = castEqual f15 <*> pure (MkT 15)
          f17 = castEqual f16 <*> pure (MkT 16)
          f18 = castEqual f17 <*> pure (MkT 17)
          f19 = castEqual f18 <*> pure (MkT 18)
          -- 20 .. 29
          f20 = castEqual f19 <*> pure (MkT 19)
          f21 = castEqual f20 <*> pure (MkT 20)
          f22 = castEqual f21 <*> pure (MkT 21)
          f23 = castEqual f22 <*> pure (MkT 22)
          f24 = castEqual f23 <*> pure (MkT 23)
          f25 = castEqual f24 <*> pure (MkT 24)
          f26 = castEqual f25 <*> pure (MkT 25)
          f27 = castEqual f26 <*> pure (MkT 26)
          f28 = castEqual f27 <*> pure (MkT 27)
          f29 = castEqual f28 <*> pure (MkT 28)
          -- 30 .. 39
          f30 = castEqual f29 <*> pure (MkT 29)
          f31 = castEqual f30 <*> pure (MkT 30)
          f32 = castEqual f31 <*> pure (MkT 31)
          f33 = castEqual f32 <*> pure (MkT 32)
          f34 = castEqual f33 <*> pure (MkT 33)
          f35 = castEqual f34 <*> pure (MkT 34)
          f36 = castEqual f35 <*> pure (MkT 35)
          f37 = castEqual f36 <*> pure (MkT 36)
          f38 = castEqual f37 <*> pure (MkT 37)
          f39 = castEqual f38 <*> pure (MkT 38)
          -- 40 .. 49
          f40 = castEqual f39 <*> pure (MkT 39)
          f41 = castEqual f40 <*> pure (MkT 40)
          f42 = castEqual f41 <*> pure (MkT 41)
          f43 = castEqual f42 <*> pure (MkT 42)
          f44 = castEqual f43 <*> pure (MkT 43)
          f45 = castEqual f44 <*> pure (MkT 44)
          f46 = castEqual f45 <*> pure (MkT 45)
          f47 = castEqual f46 <*> pure (MkT 46)
          f48 = castEqual f47 <*> pure (MkT 47)
          f49 = castEqual f48 <*> pure (MkT 48)
          -- 50 .. 59
          f50 = castEqual f49 <*> pure (MkT 49)
          f51 = castEqual f50 <*> pure (MkT 50)
          f52 = castEqual f51 <*> pure (MkT 51)
          f53 = castEqual f52 <*> pure (MkT 52)
          f54 = castEqual f53 <*> pure (MkT 53)
          f55 = castEqual f54 <*> pure (MkT 54)
          f56 = castEqual f55 <*> pure (MkT 55)
          f57 = castEqual f56 <*> pure (MkT 56)
          f58 = castEqual f57 <*> pure (MkT 57)
          f59 = castEqual f58 <*> pure (MkT 58)
          -- 60 .. 69
          f60 = castEqual f59 <*> pure (MkT 59)
          f61 = castEqual f60 <*> pure (MkT 60)
          f62 = castEqual f61 <*> pure (MkT 61)
          f63 = castEqual f62 <*> pure (MkT 62)
          f64 = castEqual f63 <*> pure (MkT 63)
          f65 = castEqual f64 <*> pure (MkT 64)
          f66 = castEqual f65 <*> pure (MkT 65)
          f67 = castEqual f66 <*> pure (MkT 66)
          f68 = castEqual f67 <*> pure (MkT 67)
          f69 = castEqual f68 <*> pure (MkT 68)
          -- 70 .. 79
          f70 = castEqual f69 <*> pure (MkT 69)
          f71 = castEqual f70 <*> pure (MkT 70)
          f72 = castEqual f71 <*> pure (MkT 71)
          f73 = castEqual f72 <*> pure (MkT 72)
          f74 = castEqual f73 <*> pure (MkT 73)
          f75 = castEqual f74 <*> pure (MkT 74)
          f76 = castEqual f75 <*> pure (MkT 75)
          f77 = castEqual f76 <*> pure (MkT 76)
          f78 = castEqual f77 <*> pure (MkT 77)
          f79 = castEqual f78 <*> pure (MkT 78)
          -- 80 .. 89
          f80 = castEqual f79 <*> pure (MkT 79)
          f81 = castEqual f80 <*> pure (MkT 80)
          f82 = castEqual f81 <*> pure (MkT 81)
          f83 = castEqual f82 <*> pure (MkT 82)
          f84 = castEqual f83 <*> pure (MkT 83)
          f85 = castEqual f84 <*> pure (MkT 84)
          f86 = castEqual f85 <*> pure (MkT 85)
          f87 = castEqual f86 <*> pure (MkT 86)
          f88 = castEqual f87 <*> pure (MkT 87)
          f89 = castEqual f88 <*> pure (MkT 88)

          res = castEqual f89 <*> pure (MkT 89)

      in res

    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
