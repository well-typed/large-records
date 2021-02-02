{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

#if PROFILE_GEN_CODE
{-# OPTIONS_GHC -fplugin=GhcDump.Plugin #-}
#endif

module Test.Record.Size.After.HK050 where

import Data.Record.TH

import Test.Record.Size.Infra

largeRecord defaultLazyOptions (higherKindedRecordOfSize 50)
