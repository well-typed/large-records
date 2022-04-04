{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE RankNTypes       #-}

-- | Simple interface (without a functor argument)
--
-- See "Data.Record.Anon.Advanced" for the advanced interface.
-- You will probably also want to import "Data.Record.Anon".
--
-- Intended for qualified import.
--
-- > import Data.Record.Anon
-- > import Data.Record.Anon.Simple (Record)
-- > import qualified Data.Record.Anon.Simple as Anon
module Data.Record.Anon.Simple (
    Record
    -- * Construction
  , empty
  , insert
  , insertA
  , applyPending
    -- * Field access
  , get
  , set
    -- * Changing rows
  , merge
  , project
  , inject
  , lens
    -- * Interop with the advanced API
  , toAdvanced
  , fromAdvanced
    -- * Experimental integration with @typelet@
    --
    -- |
    -- The @typelet@ plugin provides support for type sharing. These functions
    -- can be used to construct records that result in ghc core that is truly
    -- linear in size.
  , letRecordT
  , letInsertAs
  ) where

import TypeLet

import Data.Record.Anon

import qualified Data.Record.Anon.Advanced as A

import Data.Record.Anonymous.Simple (Record)
import qualified Data.Record.Anonymous.Simple as S

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLabels
-- >>> :set -XTypeOperators
-- >>> :set -fplugin=TypeLet -fplugin=Data.Record.Anon.Plugin
-- >>> :set -dppr-cols=200
-- >>> import TypeLet
-- >>> import Data.Record.Anon

{-------------------------------------------------------------------------------
  Construction

  See discussion in Data.Record.Anon.Advanced for why we don't simply re-export.
-------------------------------------------------------------------------------}

-- | Empty record
empty :: Record '[]
empty = S.empty

-- | Insert new field
--
-- >>> :{
-- example :: Record [ "a" := Bool, "b" := Int ]
-- example =
--      insert #a True
--    $ insert #b 1
--    $ empty
-- :}
--
-- Instead of using 'insert' and 'empty', you can also write this as
--
-- > example = ANON {
-- >       a = True
-- >     , b = 1
-- >     }
insert :: Field n -> a -> Record r -> Record (n := a : r)
insert = S.insert

-- | Applicative insert
--
-- This is a simple wrapper around 'insert', but can be quite useful when
-- constructing records. Consider code like
--
-- >>> :{
-- example :: Applicative m => m a -> m b -> m (a, b)
-- example ma mb = (,) <$> ma <*> mb
-- :}
--
-- We cannot really extend this to the world of named records, but we /can/
-- do something comparable using anonymous records:
--
-- >>> :{
-- example :: Applicative m => m a -> m b -> m (Record [ "a" := a, "b" := b ])
-- example ma mb =
--       insertA #a ma
--     $ insertA #b mb
--     $ pure empty
-- :}
--
-- However, it may be more convenient to use the advanced API for this.
-- See 'Data.Record.Anon.Advanced.insertA'.
insertA ::
     Applicative m
  => Field n -> m a -> m (Record r) -> m (Record (n := a : r))
insertA = S.insertA

-- | Apply all pending changes to the record
--
-- Updates to a record are stored in a hashtable. As this hashtable grows,
-- record field access and update will become more expensive. Applying the
-- updates, resulting in a flat vector, is an @O(n)@ operation. This will happen
-- automatically whenever another @O(n)@ operation is applied (for example,
-- mapping a function over the record). However, occassionally it is useful to
-- explicitly apply these changes, for example after constructing a record or
-- updating a lot of fields.
applyPending :: Record r -> Record r
applyPending = S.applyPending

{-------------------------------------------------------------------------------
  Field access
-------------------------------------------------------------------------------}

-- | Get field from the record
--
-- This is just a wrapper around 'getField'.
--
-- >>> :{
-- example :: Record [ "a" := Bool, "b" := Int ] -> Bool
-- example r = get #a r
-- :}
--
-- If using @record-dot-preprocessor@, you can also write this example as
--
-- > example r = r.a
--
-- See 'Data.Record.Anon.Advanced.get' for additional discussion.
get :: RowHasField n r a => Field n -> Record r -> a
get = S.get

-- | Update field in the record
--
-- This is just a wrapper around 'setField'.
--
-- >>> :{
-- example ::
--      Record [ "a" := Bool, "b" := Int ]
--   -> Record [ "a" := Bool, "b" := Int ]
-- example r = set #a False r
-- :}
--
-- If using @record-dot-preprocessor@, can also write this example as
--
-- > example r = r{a = False}
set :: RowHasField n r a => Field n -> a -> Record r -> Record r
set = S.set

{-------------------------------------------------------------------------------
  Changing rows
-------------------------------------------------------------------------------}

-- | Merge two records
--
-- The 'Merge' type family does not reduce:
--
-- >>> :{
-- example :: Record (Merge '[ "a" :=  Bool ] '[])
-- example = merge (insert #a True empty) empty
-- :}
--
-- If you want to flatten the row after merging, you can use 'project':
--
-- >>> :{
-- example :: Record '[ "a" :=  Bool ]
-- example = project $ merge (insert #a True empty) empty
-- :}
--
-- See 'Data.Record.Anon.Advanced.merge' for additional discussion.
merge :: Record r -> Record r' -> Record (Merge r r')
merge = S.merge

-- | Project from one record to another
--
-- Both the source record and the target record must be fully known.
--
-- The target record can omit fields from the source record, as well as
-- rearrange them:
--
-- >>> :{
-- example ::
--      Record [ "a" := Char, "b" := Int, "c" := Bool ]
--   -> Record [ "c" := Bool, "a" := Char ]
-- example = project
-- :}
--
-- As we saw in 'merge', 'project' can also flatten 'Merge'd rows.
-- See 'Data.Record.Anon.Advanced.project' for additional discussion.
project :: Project r r' => Record r -> Record r'
project = S.project

-- | Inject smaller record into larger record
--
-- This is just the 'lens' setter.
inject :: Project r r' => Record r' -> Record r -> Record r
inject = S.inject

-- | Lens from one record to another
--
-- See 'project' for examples ('project' is just the lens getter, without the
-- setter).
lens :: Project r r' => Record r -> (Record r', Record r' -> Record r)
lens = S.lens

{-------------------------------------------------------------------------------
  Interop with the advanced API
-------------------------------------------------------------------------------}

-- | Move from the simple to the advanced interface
--
-- This is an @O(1)@ operation.
toAdvanced :: Record r -> A.Record I r
toAdvanced = S.toAdvanced

-- | Move from the advanced to the simple interface
--
-- This is an @O(1)@ operation.
fromAdvanced :: A.Record I r -> Record r
fromAdvanced = S.fromAdvanced

{-------------------------------------------------------------------------------
  Experimental integration with @typelet@
-------------------------------------------------------------------------------}

-- | Introduce type variable for a row
--
-- This can be used in conjunction with 'letInsertAs':
--
-- >>> :{
-- example :: Record '[ "a" := Int, "b" := Char, "c" := Bool ]
-- example = letRecordT $ \p -> castEqual $
--     letInsertAs p #c True empty $ \xs02 ->
--     letInsertAs p #b 'X'  xs02  $ \xs01 ->
--     letInsertAs p #a 1    xs01  $ \xs00 ->
--     castEqual xs00
-- :}
letRecordT :: forall r.
     (forall r'. Let r' r => Proxy r' -> Record r)
  -> Record r
letRecordT f = S.letRecordT f

-- | Insert field into a record and introduce type variable for the result
letInsertAs :: forall r r' n a.
     Proxy r     -- ^ Type of the record we are constructing
  -> Field n     -- ^ New field to be inserted
  -> a           -- ^ Value of the new field
  -> Record r'   -- ^ Record constructed so far
  -> (forall r''. Let r'' (n := a : r') => Record r'' -> Record r)
                 -- ^ Assign type variable to new partial record, and continue
  -> Record r
letInsertAs p n x r f = S.letInsertAs p n x r f

