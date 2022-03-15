{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Simple interface to the anonymous records library
--
-- This module defines a type @Record r@ such that, for example,
--
-- > Record '[ '("a", Bool), '("b", Char) ]
--
-- is the type of records with two fields @a@ and @b@, of types @Bool@ and
-- @Char@ respectively. The difference between the simple interface and the
-- advanced interface is that the advanced interface defines a type
--
-- > Record f '[ '("a", Bool), '("b", Char) ]
--
-- In this case, fields @a@ and @b@ have type @f Bool@ and @f Char@ instead.
-- See "Data.Record.Anonymous.Advanced" for details.
--
-- NOTE: We do not offer a set of combinators in the simple interface, as these
-- are not likely to be very useful. In the rare cases that they are needed,
-- users should use 'toAdvanced'/'fromAdvanced' to temporary use the advanced
-- API for these operations.
--
-- This module is intended for qualified import.
--
-- > import Data.Record.Anonymous.Simple (Record)
-- > import qualified Data.Record.Anonymous.Simple as Anon
module Data.Record.Anonymous.Simple (
    Record -- opaque
    -- * Basic API
  , Field -- opaque
  , empty
  , insert
  , get
  , set
  , merge
  , castRecord
    -- * Constraints
  , RecordConstraints
    -- * Working with rows
  , Isomorphic
  , Merge
  , AllFields
  , KnownFields
    -- * Interop with the advanced interface
  , toAdvanced
  , fromAdvanced
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Record.Generic
import Data.Record.Generic.Eq
import Data.Record.Generic.JSON
import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)
import Data.Record.Generic.Show
import GHC.Exts
import GHC.Records.Compat
import GHC.TypeLits

import qualified Data.Vector as Vector

import Data.Record.Anonymous.Internal.Record (Field)
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Advanced as Adv

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Anonymous record with fields @r@
newtype Record r = SimpleRecord { toAdvanced :: Adv.Record I r }

fromAdvanced :: Adv.Record I r -> Record r
fromAdvanced = SimpleRecord

{-------------------------------------------------------------------------------
  Basic API
-------------------------------------------------------------------------------}

empty :: Record '[]
empty = fromAdvanced $ Adv.empty

insert :: Field nm -> a -> Record r -> Record ('(nm, a) ': r)
insert nm x = fromAdvanced . Adv.insert nm (I x) . toAdvanced

merge :: Record r -> Record r' -> Record (Merge r r')
merge r r' = fromAdvanced $ Adv.merge (toAdvanced r) (toAdvanced r')

castRecord :: Isomorphic r r' => Record r -> Record r'
castRecord = fromAdvanced . Adv.castRecord . toAdvanced

{-------------------------------------------------------------------------------
  HasField
-------------------------------------------------------------------------------}

instance HasField  nm            (Adv.Record I r) (I a)
      => HasField (nm :: Symbol) (    Record   r)    a where
  hasField = aux . hasField @nm . toAdvanced
    where
      aux :: (I a -> Adv.Record I r, I a) -> (a -> Record r, a)
      aux (setX, x) = (fromAdvanced . setX . I, unI x)

-- | Get field from the record
--
-- This is just a wrapper around 'getField'
get :: forall nm r a.
     HasField nm (Record r) a
  => Field nm -> Record r -> a
get _ = getField @nm @(Record r)

-- | Update field in the record
--
-- This is just a wrapper around 'setField'.
set :: forall nm r a.
     HasField nm (Record r) a
  => Field nm -> a -> Record r -> Record r
set _ = flip (setField @nm @(Record r))

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

class    (AllFields r c, KnownFields r) => RecordConstraints r c
instance (AllFields r c, KnownFields r) => RecordConstraints r c

{-------------------------------------------------------------------------------
  Generics

  We define 'dict' and 'metadata' directly rather than going through the
  instance for 'Adv.Record'; we /could/ do that, but it's hassle and doesn't
  really buy us anything.
-------------------------------------------------------------------------------}

instance KnownFields r => Generic (Record r) where
  type Constraints (Record r) = RecordConstraints r
  type MetadataOf  (Record r) = r

  from     = fromAdvancedRep . from . toAdvanced
  to       = fromAdvanced    . to   . toAdvancedRep
  dict     = Rep . fieldDicts (Proxy @r)
  metadata = const recordMetadata

fromAdvancedRep :: Rep I (Adv.Record I r) -> Rep I (Record r)
fromAdvancedRep = noInlineUnsafeCo

toAdvancedRep :: Rep I (Record r) -> Rep I (Adv.Record I r)
toAdvancedRep = noInlineUnsafeCo

recordMetadata :: forall r. KnownFields r => Metadata (Record r)
recordMetadata = Metadata {
      recordName          = "Record"
    , recordConstructor   = "Record"
    , recordSize          = length fields
    , recordFieldMetadata = Rep $ Vector.fromList fields
    }
  where
    fields :: [FieldMetadata Any]
    fields = fieldMetadata (Proxy @r)

{-------------------------------------------------------------------------------
  Instances

  As for the generic instances, we make no attempt to go through the advanced
  API here, as it's painful for little benefit.
-------------------------------------------------------------------------------}

instance RecordConstraints r Show => Show (Record r) where
  showsPrec = gshowsPrec

instance RecordConstraints r Eq => Eq (Record r) where
  (==) = geq

instance ( RecordConstraints r Eq
         , RecordConstraints r Ord
         ) => Ord (Record r) where
  compare = gcompare

instance RecordConstraints r ToJSON => ToJSON (Record r) where
  toJSON = gtoJSON

instance RecordConstraints r FromJSON => FromJSON (Record r) where
  parseJSON = gparseJSON
