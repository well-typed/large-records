{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

module Test.Record.Experiments.RecordZip.RZ080 where

import Test.Record.Size.Infra
import Test.Record.Experiments.SimpleRecord.SR080

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
        -- 31 .. 40
    <*> f (field31  r) (field31  r')
    <*> f (field32  r) (field32  r')
    <*> f (field33  r) (field33  r')
    <*> f (field34  r) (field34  r')
    <*> f (field35  r) (field35  r')
    <*> f (field36  r) (field36  r')
    <*> f (field37  r) (field37  r')
    <*> f (field38  r) (field38  r')
    <*> f (field39  r) (field39  r')
    <*> f (field40  r) (field40  r')
        -- 41 .. 50
    <*> f (field41  r) (field41  r')
    <*> f (field42  r) (field42  r')
    <*> f (field43  r) (field43  r')
    <*> f (field44  r) (field44  r')
    <*> f (field45  r) (field45  r')
    <*> f (field46  r) (field46  r')
    <*> f (field47  r) (field47  r')
    <*> f (field48  r) (field48  r')
    <*> f (field49  r) (field49  r')
    <*> f (field50  r) (field50  r')
        -- 51 .. 60
    <*> f (field51  r) (field51  r')
    <*> f (field52  r) (field52  r')
    <*> f (field53  r) (field53  r')
    <*> f (field54  r) (field54  r')
    <*> f (field55  r) (field55  r')
    <*> f (field56  r) (field56  r')
    <*> f (field57  r) (field57  r')
    <*> f (field58  r) (field58  r')
    <*> f (field59  r) (field59  r')
    <*> f (field60  r) (field60  r')
        -- 61 .. 70
    <*> f (field61  r) (field61  r')
    <*> f (field62  r) (field62  r')
    <*> f (field63  r) (field63  r')
    <*> f (field64  r) (field64  r')
    <*> f (field65  r) (field65  r')
    <*> f (field66  r) (field66  r')
    <*> f (field67  r) (field67  r')
    <*> f (field68  r) (field68  r')
    <*> f (field69  r) (field69  r')
    <*> f (field70  r) (field70  r')
        -- 71 .. 80
    <*> f (field71  r) (field71  r')
    <*> f (field72  r) (field72  r')
    <*> f (field73  r) (field73  r')
    <*> f (field74  r) (field74  r')
    <*> f (field75  r) (field75  r')
    <*> f (field76  r) (field76  r')
    <*> f (field77  r) (field77  r')
    <*> f (field78  r) (field78  r')
    <*> f (field79  r) (field79  r')
    <*> f (field80  r) (field80  r')
