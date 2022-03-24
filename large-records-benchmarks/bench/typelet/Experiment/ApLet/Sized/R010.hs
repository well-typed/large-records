#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Experiment.ApLet.Sized.R010 where

import TypeLet

import Bench.Types
import Common.FunOfArity.F010

applyF :: forall f r. Applicative f => F r -> f r
applyF f =
    -- 09 .. 00
    case letT (Proxy @(T 09 -> r))   of { LetT (_ :: Proxy l09) ->
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

          res = castEqual f09 <*> pure (MkT 09)

      in res

    }}}}}}}}}}
