module Common.FunOfArity.F020 (F) where

import Bench.Types

type F r =
       -- 00 .. 09
       T 00
    -> T 01
    -> T 02
    -> T 03
    -> T 04
    -> T 05
    -> T 06
    -> T 07
    -> T 08
    -> T 09
       -- 10 .. 19
    -> T 10
    -> T 11
    -> T 12
    -> T 13
    -> T 14
    -> T 15
    -> T 16
    -> T 17
    -> T 18
    -> T 19
    -> r
