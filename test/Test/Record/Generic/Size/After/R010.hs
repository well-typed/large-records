{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Generic.Size.After.R010 where

-- TODO: Temporary imports, should be gone after we generate HasField instances
import GHC.Records.Compat
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Vector as V

import Data.Aeson (ToJSON(..))

import Data.Record.Generic
import Data.Record.Generic.JSON
import Data.Record.Generic.TH

import Test.Record.Generic.Size.Infra

largeRecord defaultOptions (recordOfSize 10)

instance ToJSON R where
  toJSON = gtoJSON

unsafeSetIndexT :: forall x. Int -> R -> x -> R
unsafeSetIndexT n r x =
    RFromVector (V.unsafeUpd (vectorFromR r) [(n, unsafeCoerce x)])

setField1 :: R -> T 1 -> R
setField1 = unsafeSetIndexT 1

instance HasField "field1" R (T 1) where
  hasField = \r -> (setField1 r, field1 r)
