{-# LANGUAGE DataKinds #-}

module Test.Record.Anonymous.Index.AI010 where

import Data.Record.Anonymous.RedBlackTree (FromList)
import Test.Record.Size.Infra

type ExampleFields = FromList '[
     -- 00..09
      '("t00", T 00)
    , '("t01", T 01)
    , '("t02", T 02)
    , '("t03", T 03)
    , '("t04", T 04)
    , '("t05", T 05)
    , '("t06", T 06)
    , '("t07", T 07)
    , '("t08", T 08)
    , '("t09", T 09)
    ]
