module Test.Size.HList.Baseline.Baseline010 where

import Test.Infra
import Test.Size.HList.Index.Ix010

hlist :: HList Fields
hlist =
      -- 00 .. 09
      HCons (MkT @"i00")
    $ HCons (MkT @"i01")
    $ HCons (MkT @"i02")
    $ HCons (MkT @"i03")
    $ HCons (MkT @"i04")
    $ HCons (MkT @"i05")
    $ HCons (MkT @"i06")
    $ HCons (MkT @"i07")
    $ HCons (MkT @"i08")
    $ HCons (MkT @"i09")
    $ HNil

