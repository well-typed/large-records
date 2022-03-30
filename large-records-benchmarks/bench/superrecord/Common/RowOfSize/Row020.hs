{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Common.RowOfSize.Row020 (ExampleRow) where

import SuperRecord

import Bench.Types

type ExampleRow = [
    -- 00 .. 09
    "t00" := T 00
  , "t01" := T 01
  , "t02" := T 02
  , "t03" := T 03
  , "t04" := T 04
  , "t05" := T 05
  , "t06" := T 06
  , "t07" := T 07
  , "t08" := T 08
  , "t09" := T 09
    -- 10 .. 19
  , "t10" := T 10
  , "t11" := T 11
  , "t12" := T 12
  , "t13" := T 13
  , "t14" := T 14
  , "t15" := T 15
  , "t16" := T 16
  , "t17" := T 17
  , "t18" := T 18
  , "t19" := T 19
  ]

