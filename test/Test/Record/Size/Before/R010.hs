{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

#if USE_RDP
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
#endif

#if USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin=GhcDump.Plugin #-}
#endif

module Test.Record.Size.Before.R010 where

import Data.Aeson
import Generics.SOP.JSON
import Generics.SOP.TH
import GHC.Records.Compat

import Test.Record.Size.Infra

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
    }

deriving instance Eq   R
deriving instance Show R

deriveGeneric ''R

instance ToJSON R where
  toJSON = gtoJSON defaultJsonOptions

-- For the sake of testing, we define one HasField instance by hand if RDP is
-- not used. We're careful not to depend on field selectors, so that this still
-- works NoFieldSelectors.
#if !USE_RDP
instance HasField "field1" R (T 1) where
  hasField (MkR f1 f2 f3 f4 f5 f6 f7 f8 f9 f10) = (
        \f1' -> MkR f1' f2 f3 f4 f5 f6 f7 f8 f9 f10
      , f1
      )
#endif
