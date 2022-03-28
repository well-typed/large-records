-- | Advanced interface to the anonymous records library
--
-- This module is the main module for using the @large-anon@ library in its
-- full generality. Compare:
--
-- > Data.Record.Anonymous.Simple     (Record   r)
-- > Data.Record.Anonymous.Advanced   (Record f r)
--
-- Compared the the simple API, we have an additional functor argument @f@,
-- which is applied to every field in the record. In other words, @Record r@
-- from the simple interface is isomorphic to @Record I r@ here.
--
-- This module is intended for qualified import.
--
-- > import Data.Record.Anonymous.Advanced (Record)
-- > import qualified Data.Record.Anonymous.Advanced as Anon
module Data.Record.Anonymous.Advanced (
    Record -- Opaque
    -- * Basic API
  , Record.Field -- opaque
  , Record.empty
  , Record.insert
  , Record.get
  , Record.set
  , Record.merge
  , Record.lens
  , Record.project
  , Record.applyDiff
    -- * Constraints
  , RecordConstraints
  , recordOfDicts
  , constrain
    -- * Combinators
    -- ** "Functor"
  , Combinators.map
  , Combinators.mapM
  , Combinators.cmap
  , Combinators.cmapM
    -- ** Zipping
  , Combinators.zip
  , Combinators.zipWith
  , Combinators.zipWithM
  , Combinators.czipWith
  , Combinators.czipWithM
    -- ** "Foldable"
  , Combinators.collapse
  , Combinators.toList
    -- ** "Traversable"
  , Combinators.sequenceA
  , Combinators.sequenceA'
    -- ** "Applicable"
  , Combinators.pure
  , Combinators.cpure
  , Combinators.ap
    -- * Working with rows
  , Pair(..)
  , Row
  , Project
  , Merge
  , FieldTypes
  , AllFields
  , KnownFields
    -- * Additional generic functions
  , describeRecord
  , recordWithMetadata
  , recordWithNames
    -- * Support for @typelet@
  , Record.letRecordT
  , Record.letInsertAs
  ) where

import Data.Record.Anonymous.Internal.Record (Record)

import Data.Record.Anonymous.Internal.Constraints
import Data.Record.Anonymous.Internal.Row
import Data.Record.Anonymous.Internal.Generics

import qualified Data.Record.Anonymous.Internal.Combinators.Constrained as Combinators
import qualified Data.Record.Anonymous.Internal.Combinators.Simple      as Combinators
import qualified Data.Record.Anonymous.Internal.Record                  as Record


