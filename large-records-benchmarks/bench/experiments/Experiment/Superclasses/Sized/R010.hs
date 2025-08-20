{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.Superclasses.Sized.R010 where

import Bench.Types

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
