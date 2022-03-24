module Test.Size.Ap.Baseline.Baseline020 where

import Test.Infra
import Test.Size.Ap.Index.Ix020

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
