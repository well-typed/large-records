{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Test.Record.Anonymous.Construct.AC010 where

import Data.Record.Anonymous (Record)
import Data.Record.Anonymous.RedBlackTree (Insert, Empty)

import qualified Data.Record.Anonymous as A

import Test.Record.Size.Infra
import Test.Record.Anonymous.Index.AI010

import TypeLet

{-
type ExampleFields = FromList '[
     -- 00..09
      '("t00", T 00)
    , '("t01", T 01)
    , '("t02", T 02)
    -- , '("t03", T 03)
    -- , '("t04", T 04)
    -- , '("t05", T 05)
    -- , '("t06", T 06)
    -- , '("t07", T 07)
    -- , '("t08", T 08)
    -- , '("t09", T 09)
    ]
-}

exampleRecord :: Record ExampleFields
exampleRecord =
    case letT (Proxy @(Insert "t00" (T 00) Empty)) of { LetT (_ :: Proxy r00) ->
    case letT (Proxy @(Insert "t01" (T 01) r00))   of { LetT (_ :: Proxy r01) ->
    case letT (Proxy @(Insert "t02" (T 02) r01))   of { LetT (_ :: Proxy r02) ->

      let x00 :: Record r00
          x01 :: Record r01
          x02 :: Record r02

          x00 = castEqual (A.insert #t00 (MkT 0 :: T 00) A.empty)
          x01 = castEqual (A.insert #t01 (MkT 0 :: T 01) x00)
          x02 = castEqual (A.insert #t02 (MkT 0 :: T 02) x01)

      in castEqual x02

    }}}


{-
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








-}