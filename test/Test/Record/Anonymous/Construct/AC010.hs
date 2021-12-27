{-# LANGUAGE OverloadedLabels #-}

module Test.Record.Anonymous.Construct.AC010 where

import Data.Record.Anonymous (Record)

import qualified Data.Record.Anonymous as Anon

import Test.Record.Size.Infra
import Test.Record.Anonymous.Index.AI010

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
      -- Done
    $ Anon.empty








