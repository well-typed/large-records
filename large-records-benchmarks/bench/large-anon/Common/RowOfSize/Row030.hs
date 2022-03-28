{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Common.RowOfSize.Row030 (Row) where

import Data.Record.Anonymous.Simple (Pair((:=)))

import Bench.Types

type Row = '[
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
    -- 20 .. 29
  , "t20" := T 20
  , "t21" := T 21
  , "t22" := T 22
  , "t23" := T 23
  , "t24" := T 24
  , "t25" := T 25
  , "t26" := T 26
  , "t27" := T 27
  , "t28" := T 28
  , "t29" := T 29
  ]
