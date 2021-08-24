{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Code generation shared by TH and QQ
--
-- Since these can also be used by QQ, these functions cannot take 'Options'.
module Data.Record.Internal.CodeGen (
    -- * Records
    recordTypeE
  , recordConstrE
  , recordTypeT
  , recordToVectorE
  , recordFromVectorDontForceE
  , recordIndexedAccessorE
  , recordIndexedOverwriteE
  , recordUndefinedValueE
    -- * Fields
  , fieldNameE
  , fieldNameT
  , fieldTypeT
  , fieldIndexE
  , fieldUntypedAccessorE
  , fieldUntypedOverwriteE
  ) where

import Language.Haskell.TH

import Data.Record.Internal.Naming
import Data.Record.Internal.Record
import Data.Record.Internal.TH.Util
import Data.Record.TH.Config.Options (GenPatSynonym(..))

import qualified Data.Record.Internal.TH.Name as N hiding (unqualified)

{-------------------------------------------------------------------------------
  Records
-------------------------------------------------------------------------------}

-- | Name of the record as a term-level literal
recordTypeE :: Record a -> Q Exp
recordTypeE = stringE . recordType

-- | Name of the constructor as a term-level literal
recordConstrE :: Record a -> Q Exp
recordConstrE = stringE . recordConstr

-- | The saturated type of the record (that is, with all type vars applied)
recordTypeT :: N.Qualifier -> Record a -> Q Type
recordTypeT qual Record{..} =
    appsT (N.conT (N.qualify qual recordType)) $ map tyVarType recordTVars

-- | Coerce the record to the underlying @Vector Any@
recordToVectorE :: N.Qualifier -> Record a -> Q Exp
recordToVectorE qual =
    N.varE . N.qualify qual . nameRecordInternalField . recordType

-- | Construct record from the underlying @Vector Any@
--
-- This doesn't force any elements in the vector, so this can be used if
--
-- * the record has lazy fields, or
-- * we know through other means that all values are already forced.
--
-- See also 'recordFromVectorForceE'.
recordFromVectorDontForceE :: GenPatSynonym -> N.Qualifier -> Record a -> Q Exp
recordFromVectorDontForceE genPatSyn qual =
    N.conE . N.qualify qual . nameRecordInternalConstr genPatSyn . recordConstr

-- | The (unsafe) indexed field accessor
recordIndexedAccessorE :: N.Qualifier -> Record a -> Q Exp
recordIndexedAccessorE qual =
    N.varE . N.qualify qual . nameRecordIndexedAccessor . recordType

-- | The (unsafe) indexed field overwrite
recordIndexedOverwriteE :: N.Qualifier -> Record a -> Q Exp
recordIndexedOverwriteE qual =
    N.varE . N.qualify qual . nameRecordIndexedOverwrite . recordType

recordUndefinedValueE :: GenPatSynonym -> N.Qualifier -> Record a -> Q Exp
recordUndefinedValueE genPatSyn qual r =
    [| $(recordFromVectorDontForceE genPatSyn qual r) undefined |]

{-------------------------------------------------------------------------------
  Record fields
-------------------------------------------------------------------------------}

-- | Name of the field as a term-level literal
fieldNameE :: Field a -> Q Exp
fieldNameE = stringE . fieldName

-- | Name of the field as a type-level literal
fieldNameT :: Field a -> Q Type
fieldNameT = litT . strTyLit . fieldName

-- | Type of the field
fieldTypeT :: Field a -> Q Type
fieldTypeT Field{..} = return fieldType

-- | Index of the field
fieldIndexE :: Field a -> Q Exp
fieldIndexE Field{..} = litE . integerL $ fromIntegral fieldIndex

-- | The indexed field accessor, applied to this field
fieldUntypedAccessorE :: N.Qualifier -> Record a -> Field a -> Q Exp
fieldUntypedAccessorE qual r f =
    [| $(recordIndexedAccessorE qual r) $(fieldIndexE f) |]

-- | The indexed field overwrite, applied to this field
fieldUntypedOverwriteE :: N.Qualifier -> Record a -> Field a -> Q Exp
fieldUntypedOverwriteE qual r f =
    [| $(recordIndexedOverwriteE qual r) $(fieldIndexE f) |]
