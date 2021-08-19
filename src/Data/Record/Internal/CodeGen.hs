{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Code generation shared by TH and QQ
--
-- Since these can also be used by QQ, these functions cannot take 'Options'.
module Data.Record.Internal.CodeGen (
    -- * Records
    recordUnqualE
  , recordConstrE
  , recordTypeT
  , recordToVectorE
  , recordFromVectorDontForceE
  , recordIndexedAccessorE
  , recordIndexedOverwriteE
    -- * Fields
  , fieldUnqualE
  , fieldUnqualT
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
recordUnqualE :: Record a -> Q Exp
recordUnqualE = stringE . recordUnqual

-- | Name of the constructor as a term-level literal
recordConstrE :: Record a -> Q Exp
recordConstrE = stringE . recordConstr

-- | The saturated type of the record (that is, with all type vars applied)
recordTypeT :: Record () -> Q Type
recordTypeT Record{..} =
    appsT (N.conT (N.unqualified recordUnqual)) $ map tyVarType recordTVars

-- | Coerce the record to the underlying @Vector Any@
recordToVectorE :: Record () -> Q Exp
recordToVectorE =
    N.varE . N.unqualified . nameRecordInternalField . recordUnqual

-- | Construct record from the underlying @Vector Any@
--
-- This doesn't force any elements in the vector, so this can be used if
--
-- * the record has lazy fields, or
-- * we know through other means that all values are already forced.
--
-- See also 'recordFromVectorForceE'.
recordFromVectorDontForceE :: Record () -> Q Exp
recordFromVectorDontForceE =
    N.conE . N.unqualified . nameRecordInternalConstr . recordConstr

-- | The (unsafe) indexed field accessor
recordIndexedAccessorE :: Record () -> Q Exp
recordIndexedAccessorE =
    N.varE . N.unqualified . nameRecordIndexedAccessor . recordUnqual

-- | The (unsafe) indexed field overwrite
recordIndexedOverwriteE :: Record () -> Q Exp
recordIndexedOverwriteE =
    N.varE . N.unqualified . nameRecordIndexedOverwrite . recordUnqual

{-------------------------------------------------------------------------------
  Record fields
-------------------------------------------------------------------------------}

-- | Name of the field as a term-level literal
fieldUnqualE :: Field a -> Q Exp
fieldUnqualE = stringE . fieldUnqual

-- | Name of the field as a type-level literal
fieldUnqualT :: Field a -> Q Type
fieldUnqualT = litT . strTyLit . fieldUnqual

-- | Type of the field
fieldTypeT :: Field () -> Q Type
fieldTypeT Field{..} = return fieldType

-- | Index of the field
fieldIndexE :: Field () -> Q Exp
fieldIndexE Field{..} = litE . integerL $ fromIntegral fieldIndex

-- | The indexed field accessor, applied to this field
fieldUntypedAccessorE :: Record () -> Field () -> Q Exp
fieldUntypedAccessorE r f =
    [| $(recordIndexedAccessorE r) $(fieldIndexE f) |]

-- | The indexed field overwrite, applied to this field
fieldUntypedOverwriteE :: Record () -> Field () -> Q Exp
fieldUntypedOverwriteE r f =
    [| $(recordIndexedOverwriteE r) $(fieldIndexE f) |]
