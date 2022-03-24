#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.HListLetAsCase.Sized.R010 where

import TypeLet

import Bench.HList
import Bench.Types
import Common.HListOfSize.HL010

hlist :: HList Fields
hlist =
    -- 09 .. 00
    case letAs (MkT 09 :* Nil)  of { LetAs (xs09 :: HList t09) ->
    case letAs (MkT 08 :* xs09) of { LetAs (xs08 :: HList t08) ->
    case letAs (MkT 07 :* xs08) of { LetAs (xs07 :: HList t07) ->
    case letAs (MkT 06 :* xs07) of { LetAs (xs06 :: HList t06) ->
    case letAs (MkT 05 :* xs06) of { LetAs (xs05 :: HList t05) ->
    case letAs (MkT 04 :* xs05) of { LetAs (xs04 :: HList t04) ->
    case letAs (MkT 03 :* xs04) of { LetAs (xs03 :: HList t03) ->
    case letAs (MkT 02 :* xs03) of { LetAs (xs02 :: HList t02) ->
    case letAs (MkT 01 :* xs02) of { LetAs (xs01 :: HList t01) ->
    case letAs (MkT 00 :* xs01) of { LetAs (xs00 :: HList t00) ->
      castEqual xs00
    }}}}}}}}}}
