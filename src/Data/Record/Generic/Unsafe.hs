-- | Low-level functions for creating 'Generic'/'Metadata' instances
--
-- These are primarily intended for use by the TH derivation.
module Data.Record.Generic.Unsafe (
    unsafeMetadata
  ) where

import qualified Data.Text as Text
import qualified Data.Vector as V
import Unsafe.Coerce (unsafeCoerce)

import Data.Record.Generic

{-# NOINLINE unsafeMetadata #-}
unsafeMetadata :: [String] -> Rep (K FieldName) a
unsafeMetadata = Rep . V.fromList . map unsafeCoerce . map Text.pack
