{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

{-# OPTIONS_GHC -ddump-simpl #-}

module Test.Record.Experiments.Superclasses.SC010 where

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
  ) => C where