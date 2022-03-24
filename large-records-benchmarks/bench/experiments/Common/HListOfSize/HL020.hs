{-# LANGUAGE DataKinds #-}

module Common.HListOfSize.HL020 where

import Bench.Types
import Bench.HList

type ExampleFields = '[
      -- 0
      T 00
    , T 01
    , T 02
    , T 03
    , T 04
    , T 05
    , T 06
    , T 07
    , T 08
    , T 09
      -- 1
    , T 10
    , T 11
    , T 12
    , T 13
    , T 14
    , T 15
    , T 16
    , T 17
    , T 18
    , T 19
    ]

exampleValue :: HList ExampleFields
exampleValue =
       -- 0
       MkT 00
    :* MkT 01
    :* MkT 02
    :* MkT 03
    :* MkT 04
    :* MkT 05
    :* MkT 06
    :* MkT 07
    :* MkT 08
    :* MkT 09
       -- 1
    :* MkT 10
    :* MkT 11
    :* MkT 12
    :* MkT 13
    :* MkT 14
    :* MkT 15
    :* MkT 16
    :* MkT 17
    :* MkT 18
    :* MkT 19
       --
    :* Nil


