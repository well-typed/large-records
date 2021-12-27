{-# LANGUAGE OverloadedLabels #-}

module Test.Record.Anonymous.Construct.AC040 where

import Data.Record.Anonymous (Record)

import qualified Data.Record.Anonymous as Anon

import Test.Record.Size.Infra
import Test.Record.Anonymous.Index.AI040

exampleRecord :: Record ExampleFields
exampleRecord =
      -- 00..09
      Anon.insert #t00 (MkT 00)
    $ Anon.insert #t01 (MkT 01)
    $ Anon.insert #t02 (MkT 02)
    $ Anon.insert #t03 (MkT 03)
    $ Anon.insert #t04 (MkT 04)
    $ Anon.insert #t05 (MkT 05)
    $ Anon.insert #t06 (MkT 06)
    $ Anon.insert #t07 (MkT 07)
    $ Anon.insert #t08 (MkT 08)
    $ Anon.insert #t09 (MkT 09)
      -- 10..19
    $ Anon.insert #t10 (MkT 10)
    $ Anon.insert #t11 (MkT 11)
    $ Anon.insert #t12 (MkT 12)
    $ Anon.insert #t13 (MkT 13)
    $ Anon.insert #t14 (MkT 14)
    $ Anon.insert #t15 (MkT 15)
    $ Anon.insert #t16 (MkT 16)
    $ Anon.insert #t17 (MkT 17)
    $ Anon.insert #t18 (MkT 18)
    $ Anon.insert #t19 (MkT 19)
      -- 20..29
    $ Anon.insert #t20 (MkT 20)
    $ Anon.insert #t21 (MkT 21)
    $ Anon.insert #t22 (MkT 22)
    $ Anon.insert #t23 (MkT 23)
    $ Anon.insert #t24 (MkT 24)
    $ Anon.insert #t25 (MkT 25)
    $ Anon.insert #t26 (MkT 26)
    $ Anon.insert #t27 (MkT 27)
    $ Anon.insert #t28 (MkT 28)
    $ Anon.insert #t29 (MkT 29)
      -- 30..39
    $ Anon.insert #t30 (MkT 30)
    $ Anon.insert #t31 (MkT 31)
    $ Anon.insert #t32 (MkT 32)
    $ Anon.insert #t33 (MkT 33)
    $ Anon.insert #t34 (MkT 34)
    $ Anon.insert #t35 (MkT 35)
    $ Anon.insert #t36 (MkT 36)
    $ Anon.insert #t37 (MkT 37)
    $ Anon.insert #t38 (MkT 38)
    $ Anon.insert #t39 (MkT 39)
      -- Done
    $ Anon.empty








