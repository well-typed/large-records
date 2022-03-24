module Test.Size.Ap.Baseline.Baseline010 where

import Test.Infra
import Test.Size.Ap.Index.Ix010

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
