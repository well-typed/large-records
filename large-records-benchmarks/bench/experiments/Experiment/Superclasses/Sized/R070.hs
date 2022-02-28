{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.Superclasses.Sized.R070 where

import Bench.Types

class D a where

class (
    -- 1 .. 10
    D (T 01)
  , D (T 02)
  , D (T 03)
  , D (T 04)
  , D (T 05)
  , D (T 06)
  , D (T 07)
  , D (T 08)
  , D (T 09)
  , D (T 10)
    -- 11 .. 20
  , D (T 11)
  , D (T 12)
  , D (T 13)
  , D (T 14)
  , D (T 15)
  , D (T 16)
  , D (T 17)
  , D (T 18)
  , D (T 19)
  , D (T 20)
    -- 21 .. 30
  , D (T 21)
  , D (T 22)
  , D (T 23)
  , D (T 24)
  , D (T 25)
  , D (T 26)
  , D (T 27)
  , D (T 28)
  , D (T 29)
  , D (T 30)
    -- 31 .. 40
  , D (T 31)
  , D (T 32)
  , D (T 33)
  , D (T 34)
  , D (T 35)
  , D (T 36)
  , D (T 37)
  , D (T 38)
  , D (T 39)
  , D (T 40)
    -- 41 .. 50
  , D (T 41)
  , D (T 42)
  , D (T 43)
  , D (T 44)
  , D (T 45)
  , D (T 46)
  , D (T 47)
  , D (T 48)
  , D (T 49)
  , D (T 50)
    -- 51 .. 60
  , D (T 51)
  , D (T 52)
  , D (T 53)
  , D (T 54)
  , D (T 55)
  , D (T 56)
  , D (T 57)
  , D (T 58)
  , D (T 59)
  , D (T 60)
    -- 61 .. 70
  , D (T 61)
  , D (T 62)
  , D (T 63)
  , D (T 64)
  , D (T 65)
  , D (T 66)
  , D (T 67)
  , D (T 68)
  , D (T 69)
  , D (T 70)
  ) => C where