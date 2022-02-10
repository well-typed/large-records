-- | Reexports of types and functions used by generated code.
module Data.Record.Plugin.Runtime
  ( -- Vector
    Vector,
    fromList,
    toList,
    unsafeIndex,
    unsafeUpd,
    -- LargeRecord
    noInlineUnsafeCo,
    dictFor,
    Rep (Rep),
    Dict,
    Generic (Constraints, MetadataOf, from, to, dict, metadata),
    Metadata (Metadata, recordName, recordConstructor, recordSize, recordFieldMetadata),
    repFromVector,
    repToVector,
    rnfVectorAny,
    FieldMetadata (FieldMetadata),
    FieldStrictness (FieldLazy, FieldStrict),
    ThroughLRGenerics (WrapThroughLRGenerics, unwrapThroughLRGenerics),
    geq,
    gshowsPrec,
    gcompare,
    -- Misc
    Any,
    Int,
    unsafeCoerce,
    error,
    HasField (..),
    Type,
    Constraint,
    Proxy (Proxy),
    seq,
    Show (showsPrec),
    Eq ((==)),
    Ord (compare),
  )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Record.Generic
  ( FieldMetadata (FieldMetadata),
    FieldStrictness (FieldLazy, FieldStrict),
    Generic (Constraints, MetadataOf, dict, from, metadata, to),
    Metadata (Metadata, recordConstructor, recordFieldMetadata, recordName, recordSize),
  )
import Data.Record.Generic.Eq (gcompare, geq)
import Data.Record.Generic.GHC (ThroughLRGenerics (WrapThroughLRGenerics, unwrapThroughLRGenerics))
import Data.Record.Generic.Rep.Internal (Rep (Rep), noInlineUnsafeCo)
import Data.Record.Generic.Show (gshowsPrec)
import Data.Record.TH.Runtime (dictFor, repFromVector, repToVector, rnfVectorAny)
import Data.SOP.Dict (Dict)
import Data.Vector (Vector, copy, fromList, toList, unsafeIndex, unsafeUpd)
import GHC.Base (Any)
import GHC.Records.Compat (HasField (..))
import Unsafe.Coerce (unsafeCoerce)
