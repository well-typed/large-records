{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

-- | Names of the various things we generate
--
-- This is used by both TH code generation and the quasi-quoter.
module Data.Record.Internal.Naming (
    -- * Names based on the type
    nameRecordConstraintsClass
  , nameRecordConstraintsMethod
  , nameRecordIndexedAccessor
  , nameRecordIndexedOverwrite
  , nameRecordVectorFrom
  , nameRecordVectorTo
  , nameRecordView
  ) where

{-------------------------------------------------------------------------------
  Names based on the type
-------------------------------------------------------------------------------}

nameRecordConstraintsClass  :: String -> String
nameRecordConstraintsMethod :: String -> String
nameRecordIndexedAccessor   :: String -> String
nameRecordIndexedOverwrite  :: String -> String
nameRecordVectorFrom        :: String -> String
nameRecordVectorTo          :: String -> String
nameRecordView              :: String -> String

nameRecordConstraintsClass  = ("Constraints_"     ++)
nameRecordConstraintsMethod = ("dictConstraints_" ++)
nameRecordIndexedAccessor   = ("unsafeGetIndex"   ++)
nameRecordIndexedOverwrite  = ("unsafeSetIndex"   ++)
nameRecordVectorFrom        = ("vectorFrom"       ++)
nameRecordVectorTo          = ("vectorTo"         ++)
nameRecordView              = ("tupleFrom"        ++)
