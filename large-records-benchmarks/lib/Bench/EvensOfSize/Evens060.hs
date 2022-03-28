{-# LANGUAGE DataKinds #-}

module Bench.EvensOfSize.Evens060 (Evens(..), evens) where

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
      -- 30 .. 39
    , evens30 :: !(T 30)
    , evens32 :: !(T 32)
    , evens34 :: !(T 34)
    , evens36 :: !(T 36)
    , evens38 :: !(T 38)
      -- 40 .. 49
    , evens40 :: !(T 40)
    , evens42 :: !(T 42)
    , evens44 :: !(T 44)
    , evens46 :: !(T 46)
    , evens48 :: !(T 48)
      -- 50 .. 59
    , evens50 :: !(T 50)
    , evens52 :: !(T 52)
    , evens54 :: !(T 54)
    , evens56 :: !(T 56)
    , evens58 :: !(T 58)
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
      -- 30 .. 39
    , evens30 = MkT 30
    , evens32 = MkT 32
    , evens34 = MkT 34
    , evens36 = MkT 36
    , evens38 = MkT 38
      -- 40 .. 49
    , evens40 = MkT 40
    , evens42 = MkT 42
    , evens44 = MkT 44
    , evens46 = MkT 46
    , evens48 = MkT 48
      -- 50 .. 59
    , evens50 = MkT 50
    , evens52 = MkT 52
    , evens54 = MkT 54
    , evens56 = MkT 56
    , evens58 = MkT 58
    }