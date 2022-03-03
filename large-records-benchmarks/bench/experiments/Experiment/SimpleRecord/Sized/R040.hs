{-# LANGUAGE DataKinds #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.SimpleRecord.Sized.R040 where

import Bench.Types

data R = MkR {
      -- 1 .. 10
      field1  :: T 1
    , field2  :: T 2
    , field3  :: T 3
    , field4  :: T 4
    , field5  :: T 5
    , field6  :: T 6
    , field7  :: T 7
    , field8  :: T 8
    , field9  :: T 9
    , field10 :: T 10
      -- 11 .. 20
    , field11 :: T 11
    , field12 :: T 12
    , field13 :: T 13
    , field14 :: T 14
    , field15 :: T 15
    , field16 :: T 16
    , field17 :: T 17
    , field18 :: T 18
    , field19 :: T 19
    , field20 :: T 20
      -- 21 .. 30
    , field21 :: T 21
    , field22 :: T 22
    , field23 :: T 23
    , field24 :: T 24
    , field25 :: T 25
    , field26 :: T 26
    , field27 :: T 27
    , field28 :: T 28
    , field29 :: T 29
    , field30 :: T 30
      -- 31 .. 40
    , field31 :: T 31
    , field32 :: T 32
    , field33 :: T 33
    , field34 :: T 34
    , field35 :: T 35
    , field36 :: T 36
    , field37 :: T 37
    , field38 :: T 38
    , field39 :: T 39
    , field40 :: T 40
    }
