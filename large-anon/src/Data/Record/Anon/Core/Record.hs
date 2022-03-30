{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RecordWildCards #-}

-- | Core 'Record' definition
--
-- This is the low-level definition of 'Record', which lacks the phantom row
-- parameter (@r@). All functions in this module should be considered unsafe.
--
-- Intended for qualified import.
--
-- > import Data.Record.Anon.Core.Record as Core (Record(..))
-- > import qualified Data.Record.Anon.Core.Record as Core.Record
module Data.Record.Anon.Core.Record (
    -- * Definition
    Record(..)
    -- * Conversion
  , toCanonical
  , fromCanonical
    -- * Low-level field access
  , getField
  , setField
  ) where

import Data.Kind
import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)
import GHC.Exts (Any)

import Data.Record.Anon.Core.Canonical (Canonical)
import Data.Record.Anon.Core.Diff (Diff)
import Data.Record.Anon.Core.FieldName

import qualified Data.Record.Anon.Core.Canonical as Canon
import qualified Data.Record.Anon.Core.Diff      as Diff

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Anonymous record
--
-- A @Record f xs@ has a field @n@ of type @f x@ for every @(n, x)@ in @xs@.
--
-- To access fields of the record, either use the 'HasField' instances
-- (possibly using the record-dot-preprocessor to get record-dot syntax),
-- or using the simple wrappers 'get' and 'set'. The 'HasField' instances
-- are resolved by the plugin, so be sure to use
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
--
-- Let's consider a few examples. After we define
--
-- > example :: Record '[ '("a", Bool) ]
-- > example = insert #a True empty
--
-- we get
--
-- >>> get #a example -- or @example.a@ if using RecordDotSyntax
-- I True
--
-- >>> get #b example
-- ...
-- ...No instance for (HasField "b" (Record...
-- ...
--
-- >>> get #a example :: I Int
-- ...
-- ...Couldn't match...Int...Bool...
-- ...
--
-- When part of the record is not known, it might not be possible to resolve a
-- 'HasField' constraint until later. For example, in
--
-- >>> (\r -> get #x r) :: Record I '[ '(f, a), '("x", b) ] -> I b
-- ...
-- ...No instance for (HasField "x" (...
-- ...
--
-- This is important, because if @f == "x"@, this would only be sound if also
-- @a == b@. We /could/ introduce a new constraint to say precisely that, but
-- it would have little benefit; instead we just leave the 'HasField' constraint
-- unresolved until we know more about the record.
--
-- TODO: Update the docs, and mention that this still lacks the phantom param.
data Record (f :: k -> Type) = Record {
      -- | Pending changes
      recordDiff :: !(Diff f)

      -- | Number of pending changes
    , recordDiffSize :: {-# UNPACK #-} !Int

      -- | The original record
    , recordCanon :: {-# UNPACK #-} !(Canonical f)
    }

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Construct canonical form of the record (i.e., apply the internal 'Diff')
--
-- This is @O(n)@, and should be done only for operations on records that are
-- @O(n)@ /anyway/, so that the cost can be absorbed.
toCanonical :: Record f -> Canonical f
toCanonical Record{..} = Diff.apply recordDiff recordCanon

fromCanonical :: Canonical f -> Record f
fromCanonical canon = Record {
      recordDiff     = Diff.empty
    , recordDiffSize = 0
    , recordCanon    = canon
    }

{-------------------------------------------------------------------------------
  Low-level field access API

  These are used in the generated 'HasField' instances. It is the responsibility
  of the plugin to make sure that the @Int@ index and the type @a@ are correct.
-------------------------------------------------------------------------------}

getField :: Int -> FieldName -> Record f -> a
getField  i n Record{recordDiff, recordDiffSize, recordCanon}
  | recordDiffSize == 0
  = co $ Canon.getAtIndex recordCanon i

  | otherwise
  = co $ Diff.get (i, n) recordDiff recordCanon
  where
    co  :: f Any -> a
    co = noInlineUnsafeCo

setField :: Int -> FieldName -> a -> Record f -> Record f
setField i n x r@Record{recordDiff, recordDiffSize} =
    r { recordDiff     = Diff.set (i, n) (co x) recordDiff
      , recordDiffSize = recordDiffSize + 1
      }
  where
    co :: a -> f Any
    co = noInlineUnsafeCo

