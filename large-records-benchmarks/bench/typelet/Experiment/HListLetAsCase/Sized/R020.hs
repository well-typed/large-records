#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.HListLetAsCase.Sized.R020 where

import TypeLet

import Bench.HList
import Bench.Types
import Common.HListOfSize.HL020

hlist :: HList Fields
hlist =
    -- 19 .. 10
    case letAs (MkT 19 :* Nil)  of { LetAs (xs19 :: HList t19) ->
    case letAs (MkT 18 :* xs19) of { LetAs (xs18 :: HList t18) ->
    case letAs (MkT 17 :* xs18) of { LetAs (xs17 :: HList t17) ->
    case letAs (MkT 16 :* xs17) of { LetAs (xs16 :: HList t16) ->
    case letAs (MkT 15 :* xs16) of { LetAs (xs15 :: HList t15) ->
    case letAs (MkT 14 :* xs15) of { LetAs (xs14 :: HList t14) ->
    case letAs (MkT 13 :* xs14) of { LetAs (xs13 :: HList t13) ->
    case letAs (MkT 12 :* xs13) of { LetAs (xs12 :: HList t12) ->
    case letAs (MkT 11 :* xs12) of { LetAs (xs11 :: HList t11) ->
    case letAs (MkT 10 :* xs11) of { LetAs (xs10 :: HList t10) ->
    -- 09 .. 00
    case letAs (MkT 09 :* xs10) of { LetAs (xs09 :: HList t09) ->
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
    }}}}}}}}}}
