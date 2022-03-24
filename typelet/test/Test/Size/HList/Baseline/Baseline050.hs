module Test.Size.HList.Baseline.Baseline050 where

import Test.Infra
import Test.Size.HList.Index.Ix050

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
      -- 20 .. 29
    $ HCons (MkT @"i20")
    $ HCons (MkT @"i21")
    $ HCons (MkT @"i22")
    $ HCons (MkT @"i23")
    $ HCons (MkT @"i24")
    $ HCons (MkT @"i25")
    $ HCons (MkT @"i26")
    $ HCons (MkT @"i27")
    $ HCons (MkT @"i28")
    $ HCons (MkT @"i29")
      -- 30 .. 39
    $ HCons (MkT @"i30")
    $ HCons (MkT @"i31")
    $ HCons (MkT @"i32")
    $ HCons (MkT @"i33")
    $ HCons (MkT @"i34")
    $ HCons (MkT @"i35")
    $ HCons (MkT @"i36")
    $ HCons (MkT @"i37")
    $ HCons (MkT @"i38")
    $ HCons (MkT @"i39")
      -- 40 .. 49
    $ HCons (MkT @"i40")
    $ HCons (MkT @"i41")
    $ HCons (MkT @"i42")
    $ HCons (MkT @"i43")
    $ HCons (MkT @"i44")
    $ HCons (MkT @"i45")
    $ HCons (MkT @"i46")
    $ HCons (MkT @"i47")
    $ HCons (MkT @"i48")
    $ HCons (MkT @"i49")
    $ HNil

