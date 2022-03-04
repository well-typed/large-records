module Data.Record.Plugin.RuntimeNames where

import GHC (ModuleName, mkModuleName)
import OccName (mkDataOcc, mkTcOcc, mkVarOcc)
import RdrName (RdrName, mkRdrQual, mkRdrUnqual)

_name, _con, _nameT, _field :: ModuleName -> String -> RdrName
_name moduleName var = mkRdrQual moduleName (mkVarOcc var)
_con moduleName var = mkRdrQual moduleName (mkDataOcc var)
_nameT moduleName var = mkRdrQual moduleName (mkTcOcc var)
_field = _name

-- Modules

allRuntimeModules :: [ModuleName]
allRuntimeModules = [moduleRuntime, moduleGHCGeneric]

moduleRuntime, moduleGHCGeneric :: ModuleName
moduleRuntime = mkModuleName "Data.Record.Plugin.Runtime"
moduleGHCGeneric = mkModuleName "GHC.Generics"

-- Data.Vector

type_Vector, fromList, toList, unsafeIndex, unsafeUpd :: RdrName
type_Vector = _nameT moduleRuntime "Vector"
fromList = _name moduleRuntime "fromList"
toList = _name moduleRuntime "toList"
unsafeIndex = _name moduleRuntime "unsafeIndex"
unsafeUpd = _name moduleRuntime "unsafeUpd"

-- Large Records

type_Rep, type_Dict, type_Generic, type_ThroughLRGenerics :: RdrName
type_Rep = _nameT moduleRuntime "Rep"
type_Dict = _nameT moduleRuntime "Dict"
type_Generic = _nameT moduleRuntime "Generic"
type_ThroughLRGenerics = _nameT moduleRuntime "ThroughLRGenerics"

type_Constraints_unqual, type_MetadataOf_unqual, from_unqual, to_unqual, dict_unqual, metadata_unqual :: RdrName
type_Constraints_unqual = mkRdrUnqual (mkTcOcc "Constraints")
type_MetadataOf_unqual = mkRdrUnqual (mkTcOcc "MetadataOf")
from_unqual = mkRdrUnqual (mkVarOcc "from")
to_unqual = mkRdrUnqual (mkVarOcc "to")
dict_unqual = mkRdrUnqual (mkVarOcc "dict")
metadata_unqual = mkRdrUnqual (mkVarOcc "metadata")

noInlineUnsafeCo, dictFor, repFromVector, repToVector, rnfVectorAny, unwrapThroughLRGenerics, geq, gshowsPrec, gcompare :: RdrName
noInlineUnsafeCo = _name moduleRuntime "noInlineUnsafeCo"
dictFor = _name moduleRuntime "dictFor"
repFromVector = _name moduleRuntime "repFromVector"
repToVector = _name moduleRuntime "repToVector"
rnfVectorAny = _name moduleRuntime "rnfVectorAny"
unwrapThroughLRGenerics = _name moduleRuntime "unwrapThroughLRGenerics"
geq = _name moduleRuntime "geq"
gshowsPrec = _name moduleRuntime "gshowsPrec"
gcompare = _name moduleRuntime "gcompare"

con_Rep, con_Metadata, con_FieldMetadata, con_FieldLazy, con_FieldStrict, con_WrapThroughLRGenerics :: RdrName
con_Rep = _con moduleRuntime "Rep"
con_Metadata = _con moduleRuntime "Metadata"
con_FieldMetadata = _con moduleRuntime "FieldMetadata"
con_FieldLazy = _con moduleRuntime "FieldLazy"
con_FieldStrict = _con moduleRuntime "FieldStrict"
con_WrapThroughLRGenerics = _con moduleRuntime "WrapThroughLRGenerics"

field_recordName, field_recordConstructor, field_recordSize, field_recordFieldMetadata :: RdrName
field_recordName = _field moduleRuntime "recordName"
field_recordConstructor = _field moduleRuntime "recordConstructor"
field_recordSize = _field moduleRuntime "recordSize"
field_recordFieldMetadata = _field moduleRuntime "recordFieldMetadata"

-- Misc

type_Any, type_Int, type_HasField, type_Type, type_Constraint, type_Proxy, type_Show, type_Eq, type_Ord :: RdrName
type_Any = _nameT moduleRuntime "Any"
type_Int = _nameT moduleRuntime "Int"
type_HasField = _nameT moduleRuntime "HasField"
type_Type = _nameT moduleRuntime "Type"
type_Constraint = _nameT moduleRuntime "Constraint"
type_Proxy = _nameT moduleRuntime "Proxy"
type_Show = _nameT moduleRuntime "Show"
type_Eq = _nameT moduleRuntime "Eq"
type_Ord = _nameT moduleRuntime "Ord"

unsafeCoerce, error, seq :: RdrName
unsafeCoerce = _name moduleRuntime "unsafeCoerce"
error = _name moduleRuntime "error"
seq = _name moduleRuntime "seq"

con_Proxy :: RdrName
con_Proxy = _con moduleRuntime "Proxy"

showsPrec, eq, compare :: RdrName
showsPrec = mkRdrUnqual (mkVarOcc "showsPrec")
eq = mkRdrUnqual (mkVarOcc "==")
compare = mkRdrUnqual (mkVarOcc "compare")

-- GHC

type_GHC_Generic, type_GHC_Rep_unqual, _GHC_from_unqual, _GHC_to_unqual :: RdrName
type_GHC_Generic = _nameT moduleGHCGeneric "Generic"
type_GHC_Rep_unqual = mkRdrUnqual (mkTcOcc "Rep")
_GHC_from_unqual = mkRdrUnqual (mkVarOcc "from")
_GHC_to_unqual = mkRdrUnqual (mkVarOcc "to")
