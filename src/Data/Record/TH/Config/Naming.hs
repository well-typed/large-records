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

import Data.Record.TH.CodeGen.Name
import Data.Record.TH.CodeGen.View
import Data.Record.TH.Config.Options

import Language.Haskell.TH.Syntax (NameSpace(..))

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
nameRecordInternalConstr :: Options -> Record -> ConstrName 'Dynamic
nameRecordInternalConstr Options{..} Record{..} = ConstrName $
    if generatePatternSynonym
      then prefixNew "FromVector" $ recordUnqual
      else prefixNew ""           $ recordConstr

nameRecordConstraintsClass  :: Options -> Record -> Name 'Dynamic
nameRecordConstraintsMethod :: Options -> Record -> Name 'Dynamic
nameRecordIndexedAccessor   :: Options -> Record -> Name 'Dynamic
nameRecordIndexedOverwrite  :: Options -> Record -> Name 'Dynamic
nameRecordInternalField     :: Options -> Record -> Name 'Dynamic
nameRecordView              :: Options -> Record -> Name 'Dynamic

nameRecordConstraintsClass  _opts = prefixNew "Constraints_"     . recordUnqual
nameRecordConstraintsMethod _opts = prefixNew "dictConstraints_" . recordUnqual
nameRecordIndexedAccessor   _opts = prefixNew "unsafeGetIndex"   . recordUnqual
nameRecordIndexedOverwrite  _opts = prefixNew "unsafeSetIndex"   . recordUnqual
nameRecordInternalField     _opts = prefixNew "vectorFrom"       . recordUnqual
nameRecordView              _opts = prefixNew "tupleFrom"        . recordUnqual

{-------------------------------------------------------------------------------
  Option-independent names

  The quasi-quoter does not have access to the options passed to the
  'largeRecord' record declaration. Names required by the quasi-quoter can
  therefore not depend on the options.
-------------------------------------------------------------------------------}

constructNameConstructorFn :: ConstrName 'Unique -> Name 'Dynamic
constructNameConstructorFn = prefixNew "_construct_"

resolveNameConstructorFn :: ConstrName 'Global -> Name 'Global
resolveNameConstructorFn = prefixExisting "_construct_" VarName
