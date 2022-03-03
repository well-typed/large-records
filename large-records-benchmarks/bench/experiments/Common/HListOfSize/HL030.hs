{-# LANGUAGE DataKinds #-}

module Common.HListOfSize.HL030 where

import Bench.Types
import Infra.HList

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
      -- 2
    , T 20
    , T 21
    , T 22
    , T 23
    , T 24
    , T 25
    , T 26
    , T 27
    , T 28
    , T 29
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
       -- 2
    :* MkT 20
    :* MkT 21
    :* MkT 22
    :* MkT 23
    :* MkT 24
    :* MkT 25
    :* MkT 26
    :* MkT 27
    :* MkT 28
    :* MkT 29
       --
    :* Nil

