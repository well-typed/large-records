{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# OPTIONS_GHC -ddump-splices                 #-}

module Test.Record.Generic.Size.Before.R010 where

import Data.Aeson
import Generics.SOP.JSON
import Generics.SOP.TH

import Test.Record.Generic.Size.Infra

-- Write out the record by hand rather than using 'recordOfSize' for now,
-- because of we generate the record using TH, the RecordDotPreprocessor does
-- not pick it up.
data R = MkR {
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
    }

deriving instance Eq   R
deriving instance Show R

deriveGeneric ''R

instance ToJSON R where
  toJSON = gtoJSON defaultJsonOptions
