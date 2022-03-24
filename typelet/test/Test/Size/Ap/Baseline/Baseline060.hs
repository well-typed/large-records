module Test.Size.Ap.Baseline.Baseline060 where

import Test.Infra
import Test.Size.Ap.Index.Ix060

applyF :: Applicative f => F r -> f r
applyF f =
        pure f
        -- 00 .. 09
    <*> pure (MkT @"i00")
    <*> pure (MkT @"i01")
    <*> pure (MkT @"i02")
    <*> pure (MkT @"i03")
    <*> pure (MkT @"i04")
    <*> pure (MkT @"i05")
    <*> pure (MkT @"i06")
    <*> pure (MkT @"i07")
    <*> pure (MkT @"i08")
    <*> pure (MkT @"i09")
        -- 10 .. 19
    <*> pure (MkT @"i10")
    <*> pure (MkT @"i11")
    <*> pure (MkT @"i12")
    <*> pure (MkT @"i13")
    <*> pure (MkT @"i14")
    <*> pure (MkT @"i15")
    <*> pure (MkT @"i16")
    <*> pure (MkT @"i17")
    <*> pure (MkT @"i18")
    <*> pure (MkT @"i19")
        -- 20 .. 29
    <*> pure (MkT @"i20")
    <*> pure (MkT @"i21")
    <*> pure (MkT @"i22")
    <*> pure (MkT @"i23")
    <*> pure (MkT @"i24")
    <*> pure (MkT @"i25")
    <*> pure (MkT @"i26")
    <*> pure (MkT @"i27")
    <*> pure (MkT @"i28")
    <*> pure (MkT @"i29")
        -- 30 .. 39
    <*> pure (MkT @"i30")
    <*> pure (MkT @"i31")
    <*> pure (MkT @"i32")
    <*> pure (MkT @"i33")
    <*> pure (MkT @"i34")
    <*> pure (MkT @"i35")
    <*> pure (MkT @"i36")
    <*> pure (MkT @"i37")
    <*> pure (MkT @"i38")
    <*> pure (MkT @"i39")
        -- 40 .. 49
    <*> pure (MkT @"i40")
    <*> pure (MkT @"i41")
    <*> pure (MkT @"i42")
    <*> pure (MkT @"i43")
    <*> pure (MkT @"i44")
    <*> pure (MkT @"i45")
    <*> pure (MkT @"i46")
    <*> pure (MkT @"i47")
    <*> pure (MkT @"i48")
    <*> pure (MkT @"i49")
        -- 50 .. 59
    <*> pure (MkT @"i50")
    <*> pure (MkT @"i51")
    <*> pure (MkT @"i52")
    <*> pure (MkT @"i53")
    <*> pure (MkT @"i54")
    <*> pure (MkT @"i55")
    <*> pure (MkT @"i56")
    <*> pure (MkT @"i57")
    <*> pure (MkT @"i58")
    <*> pure (MkT @"i59")
