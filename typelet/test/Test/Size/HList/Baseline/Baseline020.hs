module Test.Size.HList.Baseline.Baseline020 where

import Test.Infra
import Test.Size.HList.Index.Ix020

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
      -- 10 .. 19
    $ HCons (MkT @"i10")
    $ HCons (MkT @"i11")
    $ HCons (MkT @"i12")
    $ HCons (MkT @"i13")
    $ HCons (MkT @"i14")
    $ HCons (MkT @"i15")
    $ HCons (MkT @"i16")
    $ HCons (MkT @"i17")
    $ HCons (MkT @"i18")
    $ HCons (MkT @"i19")
    $ HNil

