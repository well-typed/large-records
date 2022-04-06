-- | Names of all everything used by the generated code
--
-- This follows the structure of "Data.Record.Plugin.Runtime".
module Data.Record.Internal.Plugin.Names.Runtime (
    -- * base
    type_Any
  , type_Constraint
  , type_Eq
  , type_Int
  , type_Ord
  , type_Proxy
  , type_Show
  , type_Type
  , con_Proxy
  , unq_compare
  , unq_eq
  , unq_showsPrec
  , error
  , unsafeCoerce
    -- * primitive
  , type_SmallArray
  , smallArrayFromList
  , smallArrayToList
  , indexSmallArray
  , updateSmallArray
    -- * record-hasfield
  , type_HasField
  , unq_hasField
    -- * large-generics
  , type_Dict
  , type_Generic
  , type_Rep
  , type_ThroughLRGenerics
  , unq_type_Constraints
  , unq_type_MetadataOf
  , con_FieldMetadata
  , con_Metadata
  , con_Rep
  , con_WrapThroughLRGenerics
  , unq_from
  , unq_to
  , unq_dict
  , unq_metadata
  , gcompare
  , geq
  , gshowsPrec
  , noInlineUnsafeCo
  , recordConstructor
  , recordFieldMetadata
  , recordName
  , recordSize
  , unwrapThroughLRGenerics
    -- * Auxiliary
  , dictFor
  , repFromVector
  , repToVector
  ) where

import Prelude hiding (error, showsPrec, compare)

import Data.Record.Internal.GHC.Shim

{-------------------------------------------------------------------------------
  base
-------------------------------------------------------------------------------}

type_Any        :: LRdrName
type_Constraint :: LRdrName
type_Eq         :: LRdrName
type_Int        :: LRdrName
type_Ord        :: LRdrName
type_Proxy      :: LRdrName
type_Show       :: LRdrName
type_Type       :: LRdrName

type_Any        = nameQT "Any"
type_Constraint = nameQT "Constraint"
type_Eq         = nameQT "Eq"
type_Int        = nameQT "Int"
type_Ord        = nameQT "Ord"
type_Proxy      = nameQT "Proxy"
type_Show       = nameQT "Show"
type_Type       = nameQT "Type"

con_Proxy :: LRdrName
con_Proxy = nameQC "Proxy"

error        :: LRdrName
unsafeCoerce :: LRdrName

error        = nameQV "error"
unsafeCoerce = nameQV "unsafeCoerce"

unq_compare   :: LRdrName
unq_eq        :: LRdrName
unq_showsPrec :: LRdrName

unq_compare   = nameUV "compare"
unq_eq        = nameUV "=="
unq_showsPrec = nameUV "showsPrec"

{-------------------------------------------------------------------------------
  vector
-------------------------------------------------------------------------------}

type_SmallArray :: LRdrName
type_SmallArray = nameQT "SmallArray"

smallArrayFromList :: LRdrName
smallArrayToList   :: LRdrName
indexSmallArray    :: LRdrName
updateSmallArray   :: LRdrName

smallArrayFromList = nameQV "smallArrayFromList"
smallArrayToList   = nameQV "smallArrayToList"
indexSmallArray    = nameQV "indexSmallArray"
updateSmallArray   = nameQV "updateSmallArray"

{-------------------------------------------------------------------------------
  record-hasfield
-------------------------------------------------------------------------------}

type_HasField :: LRdrName
type_HasField = nameQT "HasField"

unq_hasField :: LRdrName
unq_hasField = nameUV "hasField"

{-------------------------------------------------------------------------------
  large-generics
-------------------------------------------------------------------------------}

type_Dict              :: LRdrName
type_Generic           :: LRdrName
type_Rep               :: LRdrName
type_ThroughLRGenerics :: LRdrName

type_Dict              = nameQT "Dict"
type_Generic           = nameQT "Generic"
type_Rep               = nameQT "Rep"
type_ThroughLRGenerics = nameQT "ThroughLRGenerics"

unq_type_MetadataOf :: LRdrName
unq_type_Constraints :: LRdrName

unq_type_Constraints = nameUT "Constraints"
unq_type_MetadataOf  = nameUT "MetadataOf"

con_FieldMetadata         :: LRdrName
con_Metadata              :: LRdrName
con_Rep                   :: LRdrName
con_WrapThroughLRGenerics :: LRdrName

con_FieldMetadata         = nameQC "FieldMetadata"
con_Metadata              = nameQC "Metadata"
con_Rep                   = nameQC "Rep"
con_WrapThroughLRGenerics = nameQC "WrapThroughLRGenerics"

unq_from     :: LRdrName
unq_to       :: LRdrName
unq_dict     :: LRdrName
unq_metadata :: LRdrName

unq_from     = nameUV "from"
unq_to       = nameUV "to"
unq_dict     = nameUV "dict"
unq_metadata = nameUV "metadata"

gcompare                :: LRdrName
geq                     :: LRdrName
gshowsPrec              :: LRdrName
noInlineUnsafeCo        :: LRdrName
recordConstructor       :: LRdrName
recordFieldMetadata     :: LRdrName
recordName              :: LRdrName
recordSize              :: LRdrName
unwrapThroughLRGenerics :: LRdrName

gcompare                = nameQV "gcompare"
geq                     = nameQV "geq"
gshowsPrec              = nameQV "gshowsPrec"
noInlineUnsafeCo        = nameQV "noInlineUnsafeCo"
recordConstructor       = nameQV "recordConstructor"
recordFieldMetadata     = nameQV "recordFieldMetadata"
recordName              = nameQV "recordName"
recordSize              = nameQV "recordSize"
unwrapThroughLRGenerics = nameQV "unwrapThroughLRGenerics"

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

dictFor       :: LRdrName
repFromVector :: LRdrName
repToVector   :: LRdrName

dictFor       = nameQV "dictFor"
repFromVector = nameQV "repFromVector"
repToVector   = nameQV "repToVector"

{-------------------------------------------------------------------------------
  Internal auxiliary

  NOTE: Unqualified names are used when generating class instances.
-------------------------------------------------------------------------------}

runtime :: ModuleName
runtime = mkModuleName "Data.Record.Plugin.Runtime"

nameQV, nameQT, nameQC :: String -> LRdrName
nameQV var = noLoc $ mkRdrQual runtime $ mkVarOcc  var
nameQT var = noLoc $ mkRdrQual runtime $ mkTcOcc   var
nameQC var = noLoc $ mkRdrQual runtime $ mkDataOcc var

nameUV, nameUT :: String -> LRdrName
nameUV var = noLoc $ mkRdrUnqual $ mkVarOcc var
nameUT var = noLoc $ mkRdrUnqual $ mkTcOcc  var
