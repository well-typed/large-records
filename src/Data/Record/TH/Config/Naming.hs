{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

-- | Names of the various things we generate
module Data.Record.TH.Config.Naming (
    -- * Option-dependent names
    nameRecordInternalConstr
  , nameRecordConstraintsClass
  , nameRecordConstraintsMethod
  , nameRecordIndexedAccessor
  , nameRecordIndexedOverwrite
  , nameRecordInternalField
  , nameRecordView
    -- * Option-independent names
  , constructNameConstructorFn
  , resolveNameConstructorFn
  ) where

import Data.Record.Internal.RecordDef
import Data.Record.Internal.TH.Name
import Data.Record.TH.Config.Options

{-------------------------------------------------------------------------------
  Option-dependent names
-------------------------------------------------------------------------------}

-- | The name of the constructor used internally
--
-- We pick this depending on whether the user enabled the generation of the
-- pattern synonym:
--
-- * If we generate the pattern synonym, then that pattern synonym should have
--   the name of the original constructor, and the name we pick here must be
--   different.
--
-- * If however we do /not/ generate the pattern synonym, we have to be a bit
--   careful. If the user wants to use the QQ infrastructure for constructing
--   or deconstructing records
--
--   > [lr| MkR { x = 5, y = True } |]
--
--   we must be able to go from that name 'MkR' to the (type-level) metadata
--   associated with the record. By picking the original, user-specified,
--   name for the constructor, we can reify it, get the type of the record,
--   and then lookup the associated type-level metadata. This will fail if the
--   name is not in scope, but that is reasonable: the original record
--   construction would also not be possible if @MkR@ is not in scope.
nameRecordInternalConstr :: Options -> RecordDef -> Name 'DataName 'Dynamic
nameRecordInternalConstr Options{..} RecordDef{..} =
    if generatePatternSynonym
      then prefixNew "FromVector" $ recordDefUnqual
      else prefixNew ""           $ recordDefConstr

nameRecordConstraintsClass  :: Options -> RecordDef -> Name 'TcClsName 'Dynamic
nameRecordConstraintsMethod :: Options -> RecordDef -> Name 'VarName   'Dynamic
nameRecordIndexedAccessor   :: Options -> RecordDef -> Name 'VarName   'Dynamic
nameRecordIndexedOverwrite  :: Options -> RecordDef -> Name 'VarName   'Dynamic
nameRecordInternalField     :: Options -> RecordDef -> Name 'VarName   'Dynamic
nameRecordView              :: Options -> RecordDef -> Name 'VarName   'Dynamic

nameRecordConstraintsClass  _opts = prefixNew "Constraints_"     . recordDefUnqual
nameRecordConstraintsMethod _opts = prefixNew "dictConstraints_" . recordDefUnqual
nameRecordIndexedAccessor   _opts = prefixNew "unsafeGetIndex"   . recordDefUnqual
nameRecordIndexedOverwrite  _opts = prefixNew "unsafeSetIndex"   . recordDefUnqual
nameRecordInternalField     _opts = prefixNew "vectorFrom"       . recordDefUnqual
nameRecordView              _opts = prefixNew "tupleFrom"        . recordDefUnqual

{-------------------------------------------------------------------------------
  Option-independent names

  The quasi-quoter does not have access to the options passed to the
  'largeRecord' record declaration. Names required by the quasi-quoter can
  therefore not depend on the options.
-------------------------------------------------------------------------------}

constructNameConstructorFn :: Name 'DataName 'Unique -> Name 'VarName 'Dynamic
constructNameConstructorFn = prefixNew "_construct_"

resolveNameConstructorFn :: Name 'DataName 'Global -> Name 'VarName 'Global
resolveNameConstructorFn = prefixExisting "_construct_" VarName
