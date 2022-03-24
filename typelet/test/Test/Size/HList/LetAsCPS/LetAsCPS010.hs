{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Test.Size.HList.LetAsCPS.LetAsCPS010 where

import TypeLet

import Test.Infra
import Test.Size.HList.Index.Ix010

hlist :: HList Fields
hlist = letT' (Proxy @Fields) $ \(_ :: Proxy r) -> castEqual $
    -- 09 .. 00
    letAs' @(HList r) (HCons (MkT @"i09") HNil) $ \(xs09 :: HList t09) ->
    letAs' @(HList r) (HCons (MkT @"i08") xs09) $ \(xs08 :: HList t08) ->
    letAs' @(HList r) (HCons (MkT @"i07") xs08) $ \(xs07 :: HList t07) ->
    letAs' @(HList r) (HCons (MkT @"i06") xs07) $ \(xs06 :: HList t06) ->
    letAs' @(HList r) (HCons (MkT @"i05") xs06) $ \(xs05 :: HList t05) ->
    letAs' @(HList r) (HCons (MkT @"i04") xs05) $ \(xs04 :: HList t04) ->
    letAs' @(HList r) (HCons (MkT @"i03") xs04) $ \(xs03 :: HList t03) ->
    letAs' @(HList r) (HCons (MkT @"i02") xs03) $ \(xs02 :: HList t02) ->
    letAs' @(HList r) (HCons (MkT @"i01") xs02) $ \(xs01 :: HList t01) ->
    letAs' @(HList r) (HCons (MkT @"i00") xs01) $ \(xs00 :: HList t00) ->
      castEqual xs00
