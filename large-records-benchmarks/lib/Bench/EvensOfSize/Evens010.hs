{-# LANGUAGE DataKinds #-}

module Bench.EvensOfSize.Evens010 (Evens(..)) where

import Bench.Types

data Evens = Evens {
    -- 00 .. 09
    evens00 :: !(T 00)
  , evens02 :: !(T 02)
  , evens04 :: !(T 04)
  , evens06 :: !(T 06)
  , evens08 :: !(T 08)
  }
