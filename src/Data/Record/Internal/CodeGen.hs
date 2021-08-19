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

import qualified Data.Record.Internal.TH.Name as N

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
recordTypeT :: Record a -> Q Type
recordTypeT Record{..} =
    appsT (N.conT (N.unqualified recordType)) $ map tyVarType recordTVars

-- | Coerce the record to the underlying @Vector Any@
recordToVectorE :: Record a -> Q Exp
recordToVectorE =
    N.varE . N.unqualified . nameRecordInternalField . recordType

-- | Construct record from the underlying @Vector Any@
--
-- This doesn't force any elements in the vector, so this can be used if
--
-- * the record has lazy fields, or
-- * we know through other means that all values are already forced.
--
-- See also 'recordFromVectorForceE'.
recordFromVectorDontForceE :: Record a -> Q Exp
recordFromVectorDontForceE =
    N.conE . N.unqualified . nameRecordInternalConstr . recordConstr

-- | The (unsafe) indexed field accessor
recordIndexedAccessorE :: Record a -> Q Exp
recordIndexedAccessorE =
    N.varE . N.unqualified . nameRecordIndexedAccessor . recordType

-- | The (unsafe) indexed field overwrite
recordIndexedOverwriteE :: Record a -> Q Exp
recordIndexedOverwriteE =
    N.varE . N.unqualified . nameRecordIndexedOverwrite . recordType

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
fieldUntypedAccessorE :: Record a -> Field a -> Q Exp
fieldUntypedAccessorE r f =
    [| $(recordIndexedAccessorE r) $(fieldIndexE f) |]

-- | The indexed field overwrite, applied to this field
fieldUntypedOverwriteE :: Record a -> Field a -> Q Exp
fieldUntypedOverwriteE r f =
    [| $(recordIndexedOverwriteE r) $(fieldIndexE f) |]
