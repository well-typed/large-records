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
  , nameConstructorFn
  ) where

import Language.Haskell.TH

import Data.Record.TH.CodeGen.Name
import Data.Record.TH.CodeGen.Util
import Data.Record.TH.CodeGen.View
import Data.Record.TH.Config.Options

{-------------------------------------------------------------------------------
  Option-dependent names
-------------------------------------------------------------------------------}

-- | The name of the constructor used internally
--
-- We pick this depending on whether the user enabled the generation of the
-- pattern synonym:
--
-- * If we generate the pattern synonym, then that pattern  synonym should have
--   the name of the original constructor, and the name we pick here must be
--   different.
--
-- * If however we do /not/ generate the pattern synonym, we pick the /original/
--   name here. We do this so that the constructor name is in scope, enabling
--   the user to write TH splices such as
--
--   > $(constructRecord [| MkR { x = 5, y = True } |])
--
--   For some reason (not sure why), this is not possible unless 'MkR' exists
--   (it doesn't need to have the right type, it just needs to exist).
nameRecordInternalConstr :: Options -> Record -> Name
nameRecordInternalConstr Options{..} Record{..}
  | generatePatternSynonym = nameWithSuffix "FromVector" $ recordUnqual
  | otherwise              = name                        $ recordConstr

nameRecordConstraintsClass  :: Options -> Record -> Name
nameRecordConstraintsMethod :: Options -> Record -> Name
nameRecordIndexedAccessor   :: Options -> Record -> Name
nameRecordIndexedOverwrite  :: Options -> Record -> Name
nameRecordInternalField     :: Options -> Record -> Name
nameRecordView              :: Options -> Record -> Name

nameRecordConstraintsClass  _opts = nameWithPrefix "Constraints_"     . recordUnqual
nameRecordConstraintsMethod _opts = nameWithPrefix "dictConstraints_" . recordUnqual
nameRecordIndexedAccessor   _opts = nameWithPrefix "unsafeGetIndex"   . recordUnqual
nameRecordIndexedOverwrite  _opts = nameWithPrefix "unsafeSetIndex"   . recordUnqual
nameRecordInternalField     _opts = nameWithPrefix "vectorFrom"       . recordUnqual
nameRecordView              _opts = nameWithPrefix "tupleFrom"        . recordUnqual

{-------------------------------------------------------------------------------
  Option-independent names

  The quasi-quoter does not have access to the options passed to the
  'largeRecord' record declaration. Names required by the quasi-quoter can
  therefore not depend on the options.
-------------------------------------------------------------------------------}

nameConstructorFn :: ConstrName -> Name
nameConstructorFn (ConstrName n) = mkName $ firstToLower n
