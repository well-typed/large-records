{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Generic.Size.After.R090 where

import Data.Aeson (ToJSON(..))

import Data.Record.Generic
import Data.Record.Generic.JSON
import Data.Record.Generic.TH

import Test.Record.Generic.Size.Infra

largeRecord defaultOptions (recordOfSize 90)

instance ToJSON R where
  toJSON = gtoJSON
