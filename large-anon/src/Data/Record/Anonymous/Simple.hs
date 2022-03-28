{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
  , insertA
  , get
  , set
  , merge
  , lens
  , project
  , applyDiff
    -- * Constraints
  , RecordConstraints
    -- * Working with rows
  , Pair(..)
  , Row
  , Project
  , Merge
  , AllFields
  , KnownFields
  , SimpleFieldTypes
    -- * Interop with the advanced interface
  , toAdvanced
  , fromAdvanced
    -- * Support for @typelet@
  , letRecordT
  , letInsertAs
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
import TypeLet

import qualified Data.Vector.Generic as Vector

import Data.Record.Anonymous.Internal.Record (Field)
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Advanced as Adv
import Data.Bifunctor

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

insert :: Field n -> a -> Record r -> Record (n := a : r)
insert n x = fromAdvanced . Adv.insert n (I x) . toAdvanced

insertA ::
     Applicative m
  => Field n -> m a -> m (Record r) -> m (Record (n := a : r))
insertA f x r = insert f <$> x <*> r

merge :: Record r -> Record r' -> Record (Merge r r')
merge r r' = fromAdvanced $ Adv.merge (toAdvanced r) (toAdvanced r')

lens :: Project I r r' => Record r -> (Record r', Record r' -> Record r)
lens =
      bimap fromAdvanced (\f -> fromAdvanced . f . toAdvanced)
    . Adv.lens
    . toAdvanced

project :: Project I r r' => Record r -> Record r'
project = fst . lens

applyDiff :: Record r -> Record r
applyDiff = fromAdvanced . Adv.applyDiff . toAdvanced

{-------------------------------------------------------------------------------
  HasField
-------------------------------------------------------------------------------}

instance HasField  n            (Adv.Record I r) (I a)
      => HasField (n :: Symbol) (    Record   r)    a where
  hasField = aux . hasField @n . toAdvanced
    where
      aux :: (I a -> Adv.Record I r, I a) -> (a -> Record r, a)
      aux (setX, x) = (fromAdvanced . setX . I, unI x)

-- | Get field from the record
--
-- This is just a wrapper around 'getField'.
get :: forall n r a.
     HasField n (Record r) a
  => Field n -> Record r -> a
get _ = getField @n @(Record r)

-- | Update field in the record
--
-- This is just a wrapper around 'setField'.
set :: forall n r a.
     HasField n (Record r) a
  => Field n -> a -> Record r -> Record r
set _ = flip (setField @n @(Record r))

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
  type MetadataOf  (Record r) = SimpleFieldTypes r

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

{-------------------------------------------------------------------------------
  Support for @typelet@
-------------------------------------------------------------------------------}

-- | Introduce type variable for a row
--
-- This can be used in conjunction with 'letInsertAs':
--
-- > example :: Record '[ "a" := Int, "b" := Char, "c" := Bool ]
-- > example = letRecordT $ \p -> castEqual $
-- >     letInsertAs p #c True empty $ \xs02 ->
-- >     letInsertAs p #b 'X'  xs02  $ \xs01 ->
-- >     letInsertAs p #a 1    xs01  $ \xs00 ->
-- >     castEqual xs00
letRecordT :: forall r.
     (forall r'. Let r' r => Proxy r' -> Record r)
  -> Record r
letRecordT f = letT' (Proxy @r) f

-- | Insert field into a record and introduce type variable for the result
letInsertAs :: forall r r' n a.
     Proxy r     -- ^ Type of the record we are constructing
  -> Field n     -- ^ New field to be inserted
  -> a           -- ^ Value of the new field
  -> Record r'   -- ^ Record constructed so far
  -> (forall r''. Let r'' (n := a : r') => Record r'' -> Record r)
                 -- ^ Assign type variable to new partial record, and continue
  -> Record r
letInsertAs _ n x r = letAs' (insert n x r)

