{-# LANGUAGE RankNTypes #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.Applicative.Sized.R010 where

import Bench.Types

import Experiment.SimpleRecord.Sized.R010

zipRecordWith ::
     Applicative f
  => (forall n. T n -> T n -> f (T n))
  -> R -> R -> f R
zipRecordWith f r r' =
        pure MkR
        -- 1 .. 10
    <*> f (field1  r) (field1  r')
    <*> f (field2  r) (field2  r')
    <*> f (field3  r) (field3  r')
    <*> f (field4  r) (field4  r')
    <*> f (field5  r) (field5  r')
    <*> f (field6  r) (field6  r')
    <*> f (field7  r) (field7  r')
    <*> f (field8  r) (field8  r')
    <*> f (field9  r) (field9  r')
    <*> f (field10 r) (field10 r')
