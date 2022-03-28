{-# LANGUAGE DataKinds #-}

module Bench.EvensOfSize.Evens020 (Evens(..)) where

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
  }
