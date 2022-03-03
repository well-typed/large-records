{-# LANGUAGE RankNTypes #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.Applicative.Sized.R030 where

import Bench.Types

import Experiment.SimpleRecord.Sized.R030

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
        -- 11 .. 20
    <*> f (field11  r) (field11  r')
    <*> f (field12  r) (field12  r')
    <*> f (field13  r) (field13  r')
    <*> f (field14  r) (field14  r')
    <*> f (field15  r) (field15  r')
    <*> f (field16  r) (field16  r')
    <*> f (field17  r) (field17  r')
    <*> f (field18  r) (field18  r')
    <*> f (field19  r) (field19  r')
    <*> f (field20  r) (field20  r')
        -- 21 .. 30
    <*> f (field21  r) (field21  r')
    <*> f (field22  r) (field22  r')
    <*> f (field23  r) (field23  r')
    <*> f (field24  r) (field24  r')
    <*> f (field25  r) (field25  r')
    <*> f (field26  r) (field26  r')
    <*> f (field27  r) (field27  r')
    <*> f (field28  r) (field28  r')
    <*> f (field29  r) (field29  r')
    <*> f (field30  r) (field30  r')
