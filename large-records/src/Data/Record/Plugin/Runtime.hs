{-# LANGUAGE ConstraintKinds #-}

-- | Re-exports of types and functions used by generated code
--
-- This exports all functionality required by the generated code, with the
-- exception of GHC generics (name clash with @large-records@ generics).
--
-- This follows the structure of "Data.Record.Internal.Plugin.RuntimeNames".
module Data.Record.Plugin.Runtime (
    -- * base
    Any
  , Constraint
  , Eq((==))
  , Int
  , Ord(compare)
  , Proxy(Proxy)
  , Show(showsPrec)
  , Type
  , unsafeCoerce
  , error
    -- * vector
  , SmallArray
  , smallArrayFromList
  , smallArrayToList
  , indexSmallArray
  , updateSmallArray
    -- * record-hasfield
  , GRC.HasField(hasField)
    -- * large-generics
  , LR.Dict
  , LR.FieldMetadata(FieldMetadata)
  , LR.Generic(Constraints, MetadataOf, dict, from, metadata, to)
  , LR.Metadata(Metadata, recordConstructor, recordFieldMetadata, recordName, recordSize)
  , LR.Rep(Rep)
  , LR.ThroughLRGenerics(WrapThroughLRGenerics, unwrapThroughLRGenerics)
  , LR.gcompare
  , LR.geq
  , LR.gshowsPrec
  , LR.noInlineUnsafeCo
    -- * Auxiliary
  , dictFor
  , repFromVector
  , repToVector
  ) where

import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import Data.Primitive.SmallArray
import Data.Proxy (Proxy(Proxy))
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Foldable                    as Foldable
import qualified Data.Record.Generic              as LR
import qualified Data.Record.Generic.Eq           as LR
import qualified Data.Record.Generic.GHC          as LR
import qualified Data.Record.Generic.Rep.Internal as LR
import qualified Data.Record.Generic.Show         as LR
import qualified GHC.Records.Compat               as GRC

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

dictFor :: c x => Proxy c -> Proxy x -> LR.Dict c x
dictFor _ _ = LR.Dict

repFromVector :: SmallArray Any -> LR.Rep LR.I a
repFromVector = coerce

repToVector :: LR.Rep LR.I a -> SmallArray Any
repToVector = coerce

smallArrayToList :: SmallArray a -> [a]
smallArrayToList = Foldable.toList

updateSmallArray :: SmallArray a -> [(Int, a)] -> SmallArray a
updateSmallArray v updates = runSmallArray $ do
    v' <- thawSmallArray v 0 (sizeofSmallArray v)
    forM_ updates $ \(i, a) -> do
      writeSmallArray v' i a
    return v'
