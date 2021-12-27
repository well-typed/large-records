{-# LANGUAGE OverloadedLabels #-}

module Test.Record.Anonymous.Construct.AC020 where

import Data.Record.Anonymous (Record)

import qualified Data.Record.Anonymous as Anon

import Test.Record.Size.Infra
import Test.Record.Anonymous.Index.AI020

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
      -- Done
    $ Anon.empty








