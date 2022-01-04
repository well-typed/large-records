{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

module Test.Record.Experiments.RecordZip.RZ010 where

import Test.Record.Size.Infra
import Test.Record.Experiments.SimpleRecord.SR010

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
