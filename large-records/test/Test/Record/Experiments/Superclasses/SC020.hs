{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

{-# OPTIONS_GHC -ddump-simpl #-}

module Test.Record.Experiments.Superclasses.SC020 where

import Test.Record.Size.Infra

class D a where

class (
    -- 1 .. 10
    D (T 01)
  , D (T 02)
  , D (T 03)
  , D (T 04)
  , D (T 05)
  , D (T 06)
  , D (T 07)
  , D (T 08)
  , D (T 09)
  , D (T 10)
    -- 11 .. 20
  , D (T 11)
  , D (T 12)
  , D (T 13)
  , D (T 14)
  , D (T 15)
  , D (T 16)
  , D (T 17)
  , D (T 18)
  , D (T 19)
  , D (T 20)
  ) => C where