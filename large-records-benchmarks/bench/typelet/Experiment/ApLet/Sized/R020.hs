#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.ApLet.Sized.R020 where

import TypeLet

import Bench.Types
import Common.FunOfArity.F020

applyF :: forall f r. Applicative f => F r -> f r
applyF f =
    -- 19 .. 10
    case letT (Proxy @(T 19 -> r))   of { LetT (_ :: Proxy l19) ->
    case letT (Proxy @(T 18 -> l19)) of { LetT (_ :: Proxy l18) ->
    case letT (Proxy @(T 17 -> l18)) of { LetT (_ :: Proxy l17) ->
    case letT (Proxy @(T 16 -> l17)) of { LetT (_ :: Proxy l16) ->
    case letT (Proxy @(T 15 -> l16)) of { LetT (_ :: Proxy l15) ->
    case letT (Proxy @(T 14 -> l15)) of { LetT (_ :: Proxy l14) ->
    case letT (Proxy @(T 13 -> l14)) of { LetT (_ :: Proxy l13) ->
    case letT (Proxy @(T 12 -> l13)) of { LetT (_ :: Proxy l12) ->
    case letT (Proxy @(T 11 -> l12)) of { LetT (_ :: Proxy l11) ->
    case letT (Proxy @(T 10 -> l11)) of { LetT (_ :: Proxy l10) ->
    -- 09 .. 00
    case letT (Proxy @(T 09 -> l10)) of { LetT (_ :: Proxy l09) ->
    case letT (Proxy @(T 08 -> l09)) of { LetT (_ :: Proxy l08) ->
    case letT (Proxy @(T 07 -> l08)) of { LetT (_ :: Proxy l07) ->
    case letT (Proxy @(T 06 -> l07)) of { LetT (_ :: Proxy l06) ->
    case letT (Proxy @(T 05 -> l06)) of { LetT (_ :: Proxy l05) ->
    case letT (Proxy @(T 04 -> l05)) of { LetT (_ :: Proxy l04) ->
    case letT (Proxy @(T 03 -> l04)) of { LetT (_ :: Proxy l03) ->
    case letT (Proxy @(T 02 -> l03)) of { LetT (_ :: Proxy l02) ->
    case letT (Proxy @(T 01 -> l02)) of { LetT (_ :: Proxy l01) ->
    case letT (Proxy @(T 00 -> l01)) of { LetT (_ :: Proxy l00) ->

      let -- 00 .. 09
          f00 :: f l00
          f01 :: f l01
          f02 :: f l02
          f03 :: f l03
          f04 :: f l04
          f05 :: f l05
          f06 :: f l06
          f07 :: f l07
          f08 :: f l08
          f09 :: f l09
          -- 10 .. 19
          f10 :: f l10
          f11 :: f l11
          f12 :: f l12
          f13 :: f l13
          f14 :: f l14
          f15 :: f l15
          f16 :: f l16
          f17 :: f l17
          f18 :: f l18
          f19 :: f l19

          res :: f r

          -- 00 .. 09
          f00 = pure (castEqual f)
          f01 = castEqual f00 <*> pure (MkT 00)
          f02 = castEqual f01 <*> pure (MkT 01)
          f03 = castEqual f02 <*> pure (MkT 02)
          f04 = castEqual f03 <*> pure (MkT 03)
          f05 = castEqual f04 <*> pure (MkT 04)
          f06 = castEqual f05 <*> pure (MkT 05)
          f07 = castEqual f06 <*> pure (MkT 06)
          f08 = castEqual f07 <*> pure (MkT 07)
          f09 = castEqual f08 <*> pure (MkT 08)
          -- 10 .. 19
          f10 = castEqual f09 <*> pure (MkT 09)
          f11 = castEqual f10 <*> pure (MkT 10)
          f12 = castEqual f11 <*> pure (MkT 11)
          f13 = castEqual f12 <*> pure (MkT 12)
          f14 = castEqual f13 <*> pure (MkT 13)
          f15 = castEqual f14 <*> pure (MkT 14)
          f16 = castEqual f15 <*> pure (MkT 15)
          f17 = castEqual f16 <*> pure (MkT 16)
          f18 = castEqual f17 <*> pure (MkT 17)
          f19 = castEqual f18 <*> pure (MkT 18)

          res = castEqual f19 <*> pure (MkT 19)

      in res

    }}}}}}}}}}
    }}}}}}}}}}
