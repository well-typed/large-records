{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# OPTIONS_GHC -fplugin=GhcDump.Plugin        #-}

module Test.Record.Generic.Size.Before.R060 where

import Data.Aeson
import Generics.SOP.JSON
import Generics.SOP.TH

import Test.Record.Generic.Size.Infra

-- @RecordDotPreprocessor@ runs before TH, so cannot use 'recordOfSize'.
data R = MkR {
      -- 1 .. 10
      field1  :: T 1
    , field2  :: T 2
    , field3  :: T 3
    , field4  :: T 4
    , field5  :: T 5
    , field6  :: T 6
    , field7  :: T 7
    , field8  :: T 8
    , field9  :: T 9
    , field10 :: T 10
      -- 11 .. 20
    , field11 :: T 11
    , field12 :: T 12
    , field13 :: T 13
    , field14 :: T 14
    , field15 :: T 15
    , field16 :: T 16
    , field17 :: T 17
    , field18 :: T 18
    , field19 :: T 19
    , field20 :: T 20
      -- 21 .. 30
    , field21 :: T 21
    , field22 :: T 22
    , field23 :: T 23
    , field24 :: T 24
    , field25 :: T 25
    , field26 :: T 26
    , field27 :: T 27
    , field28 :: T 28
    , field29 :: T 29
    , field30 :: T 30
      -- 31 .. 40
    , field31 :: T 31
    , field32 :: T 32
    , field33 :: T 33
    , field34 :: T 34
    , field35 :: T 35
    , field36 :: T 36
    , field37 :: T 37
    , field38 :: T 38
    , field39 :: T 39
    , field40 :: T 40
      -- 41 .. 50
    , field41 :: T 41
    , field42 :: T 42
    , field43 :: T 43
    , field44 :: T 44
    , field45 :: T 45
    , field46 :: T 46
    , field47 :: T 47
    , field48 :: T 48
    , field49 :: T 49
    , field50 :: T 50
      -- 51 .. 60
    , field51 :: T 51
    , field52 :: T 52
    , field53 :: T 53
    , field54 :: T 54
    , field55 :: T 55
    , field56 :: T 56
    , field57 :: T 57
    , field58 :: T 58
    , field59 :: T 59
    , field60 :: T 60
    }

deriving instance Eq   R
deriving instance Show R

deriveGeneric ''R

instance ToJSON R where
  toJSON = gtoJSON defaultJsonOptions
