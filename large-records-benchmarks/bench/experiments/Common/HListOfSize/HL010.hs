{-# LANGUAGE DataKinds #-}

module Common.HListOfSize.HL010 where

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
    :* Nil

