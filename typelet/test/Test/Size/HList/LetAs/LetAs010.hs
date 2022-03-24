{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Test.Size.HList.LetAs.LetAs010 where

import TypeLet

import Test.Infra
import Test.Size.HList.Index.Ix010

hlist :: HList Fields
hlist =
    -- 09 .. 00
    case letAs (HCons (MkT @"i09") HNil) of { LetAs (xs09 :: HList t09) ->
    case letAs (HCons (MkT @"i08") xs09) of { LetAs (xs08 :: HList t08) ->
    case letAs (HCons (MkT @"i07") xs08) of { LetAs (xs07 :: HList t07) ->
    case letAs (HCons (MkT @"i06") xs07) of { LetAs (xs06 :: HList t06) ->
    case letAs (HCons (MkT @"i05") xs06) of { LetAs (xs05 :: HList t05) ->
    case letAs (HCons (MkT @"i04") xs05) of { LetAs (xs04 :: HList t04) ->
    case letAs (HCons (MkT @"i03") xs04) of { LetAs (xs03 :: HList t03) ->
    case letAs (HCons (MkT @"i02") xs03) of { LetAs (xs02 :: HList t02) ->
    case letAs (HCons (MkT @"i01") xs02) of { LetAs (xs01 :: HList t01) ->
    case letAs (HCons (MkT @"i00") xs01) of { LetAs (xs00 :: HList t00) ->
      castEqual xs00
    }}}}}}}}}}
