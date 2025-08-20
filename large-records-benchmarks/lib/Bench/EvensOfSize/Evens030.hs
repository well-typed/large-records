{-# LANGUAGE DataKinds #-}

module Bench.EvensOfSize.Evens030 (Evens(..), evens) where

import Bench.Types

data Evens = Evens {
      -- 00 .. 09
      evens00 :: !(T 00)
    , evens02 :: !(T 02)
    , evens04 :: !(T 04)
    , evens06 :: !(T 06)
    , evens08 :: !(T 08)
      -- 10 .. 19
    , evens10 :: !(T 10)
    , evens12 :: !(T 12)
    , evens14 :: !(T 14)
    , evens16 :: !(T 16)
    , evens18 :: !(T 18)
      -- 20 .. 29
    , evens20 :: !(T 20)
    , evens22 :: !(T 22)
    , evens24 :: !(T 24)
    , evens26 :: !(T 26)
    , evens28 :: !(T 28)
    }

evens :: Evens
evens = Evens {
      -- 00 .. 09
      evens00 = MkT 00
    , evens02 = MkT 02
    , evens04 = MkT 04
    , evens06 = MkT 06
    , evens08 = MkT 08
      -- 10 .. 19
    , evens10 = MkT 10
    , evens12 = MkT 12
    , evens14 = MkT 14
    , evens16 = MkT 16
    , evens18 = MkT 18
      -- 20 .. 29
    , evens20 = MkT 20
    , evens22 = MkT 22
    , evens24 = MkT 24
    , evens26 = MkT 26
    , evens28 = MkT 28
    }
