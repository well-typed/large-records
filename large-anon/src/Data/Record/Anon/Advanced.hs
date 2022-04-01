{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

-- | Advanced interface (with a functor argument)
--
-- See "Data.Record.Anon.Simple" for the simple interface.
-- You will probably also want to import "Data.Record.Anon".
--
-- Intended for qualified import.
--
-- > import Data.Record.Anon
-- > import Data.Record.Anon.Advanced (Record)
-- > import qualified Data.Record.Anon.Advanced as Anon
module Data.Record.Anon.Advanced (
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
  , lens
    -- * Combinators
    -- ** " Functor "
  , map
  , cmap
    -- ** " Applicable "
  , pure
  , cpure
  , ap
    -- ** " Foldable "
  , collapse
  , toList
    -- ** " Traversable "
  , mapM
  , cmapM
  , sequenceA
  , sequenceA'
    -- ** Zipping
  , zip
  , zipWith
  , zipWithM
  , czipWith
  , czipWithM
    -- * Constraint reification and reflection
  , reifyAllFields
  , reflectKnownFields
  , reifyKnownFields
  , reflectAllFields
  , CannotProject(..)
  , reflectProject
    -- * Existential records
  , someRecord
    -- * Experimental integration with @typelet@
    --
    -- |
    -- The @typelet@ plugin provides support for type sharing. These functions
    -- can be used to construct records that result in ghc core that is truly
    -- linear in size.
  , letRecordT
  , letInsertAs
  ) where

import Prelude hiding (sequenceA, map, mapM, pure, zip, zipWith)

import TypeLet

import Data.Record.Anon

import Data.Record.Anonymous.Advanced (Record)

import qualified Data.Record.Anonymous.Advanced  as A
import qualified Data.Record.Anonymous.Discovery as A
import Data.Record.Anonymous.Internal.Row.KnownRow (CannotProject(..))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLabels
-- >>> :set -XTypeOperators
-- >>> :set -fplugin=TypeLet -fplugin=Data.Record.Anon.Plugin
-- >>> :set -dppr-cols=200
-- >>> import Prelude hiding (pure)
-- >>> import qualified Prelude
-- >>> import Data.Record.Anon
-- >>> import TypeLet

{-------------------------------------------------------------------------------
  Construction

  Here and elsewhere, we don't just re-export the function, but instead provide
  an alias for it. This means that all user-facing documentation and,
  importantly, all docspec tests, are in public modules.

  Unfortunately, docspec (being ghci-based) cannot take advantage of the source
  plugin, so we cannot give testable examples using ANON_F.
-------------------------------------------------------------------------------}

-- | Empty record
empty :: Record f '[]
empty = A.empty

-- | Insert new field
--
-- >>> :{
-- example :: Record Maybe [ "a" := Bool, "b" := Int ]
-- example =
--      insert #a (Just True)
--    $ insert #b Nothing
--    $ empty
-- :}
--
-- Instead of using 'insert' and 'empty', you can also write this as
--
-- > example = ANON_F {
-- >       a = Just True
-- >     , b = Nothing
-- >     }
insert :: Field n -> f a -> Record f r -> Record f (n := a : r)
insert = A.insert

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
-- example ::
--      Applicative m
--   => m (f a) -> m (f b) -> m (Record f [ "a" := a, "b" := b ])
-- example ma mb =
--       insertA #a ma
--     $ insertA #b mb
--     $ Prelude.pure empty
-- :}
--
-- As for regular 'insert', this example too can instead be written using
-- @ANON_F@ and 'sequenceA' (or 'sequenceA'').
--
-- > example ma mb = sequenceA $ ANON_F {
-- >       a = Comp ma
-- >     , b = Comp mb
-- >     }
insertA ::
     Applicative m
  => Field n -> m (f a) -> m (Record f r) -> m (Record f (n ':= a : r))
insertA = A.insertA

-- | Apply all pending changes to the record
--
-- Updates to a record are stored in a hashtable. As this hashtable grows,
-- record field access and update will become more expensive. Applying the
-- updates, resulting in a flat vector, is an @O(n)@ operation. This will happen
-- automatically whenever another @O(n)@ operation is applied (for example,
-- mapping a function over the record). However, occassionally it is useful to
-- explicitly apply these changes, for example after constructing a record or
-- updating a lot of fields.
applyPending :: Record f r -> Record f r
applyPending = A.applyPending

{-------------------------------------------------------------------------------
  Field access
-------------------------------------------------------------------------------}

-- | Get field from the record
--
-- This is just a wrapper around 'getField'.
--
-- >>> :{
-- example :: Record Maybe [ "a" := Bool, "b" := Int ] -> Maybe Bool
-- example r = get #a r
-- :}
--
-- If using @record-dot-preprocessor@, you can also write this example as
--
-- > example r = r.a
--
-- If the field does not exist, you will get a type error about an unsolvable
-- 'RecordHasField' constraint:
--
-- >>> :{
-- absentField :: Record Maybe [ "a" := Bool, "b" := Int ] -> Maybe Char
-- absentField r = get #c r
-- :}
-- ...
-- ...No instance for (RecordHasField "c"...
-- ...
--
-- Type mismatches will result in regular type errors:
--
-- >>> :{
-- wrongType :: Record Maybe [ "a" := Bool, "b" := Int ] -> Maybe Char
-- wrongType r = get #a r
-- :}
-- ...
-- ...Couldn't match...Char...Bool...
-- ...
--
-- When part of the record is not known, it might not be possible to resolve a
-- 'HasField' constraint until later. For example, in
--
-- >>> :{
-- unknownField :: Record Maybe [ x := Bool, "b" := Int ] -> Maybe Int
-- unknownField r = get #b r
-- :}
-- ...
-- ...No instance for (RecordHasField "b"...
-- ...
--
-- (Note that @x@ here is a variable, not a string.) It is important that the
-- constraint remains unsolved in this example, because if @x == "b"@, the first
-- field would shadow the second, and the result type should be @Maybe Bool@
-- instead of @Maybe Int@.
get :: HasField n (Record f r) (f a) => Field n -> Record f r -> f a
get = A.get

-- | Update field in the record
--
-- This is just a wrapper around 'setField'.
--
-- >>> :{
-- example ::
--      Record Maybe [ "a" := Bool, "b" := Int ]
--   -> Record Maybe [ "a" := Bool, "b" := Int ]
-- example r = set #a (Just False) r
-- :}
--
-- If using @record-dot-preprocessor@, can also write this example as
--
-- > example r = r{a = Just False}
set ::
     HasField n (Record f r) (f a)
  => Field n -> f a -> Record f r -> Record f r
set = A.set

{-------------------------------------------------------------------------------
  Changing rows
-------------------------------------------------------------------------------}

-- | Merge two records
--
-- The 'Merge' type family does not reduce:
--
-- >>> :{
-- example :: Record Maybe (Merge '[ "a" :=  Bool ] '[])
-- example = merge (insert #a (Just True) empty) empty
-- :}
--
-- If you want to flatten the row after merging, you can use 'project':
--
-- >>> :{
-- example :: Record Maybe '[ "a" :=  Bool ]
-- example = project $ merge (insert #a (Just True) empty) empty
-- :}
--
-- 'HasField' constraints can be resolved for merged records, subject to the
-- same condition discussed in 'get': all fields in the record must be known up
-- to the requested field (in case of shadowing). So the record /may/ be fully
-- known:
--
-- >>> :{
-- example :: Record f (Merge '[ "a" := Bool ] '[ "b" := Char ]) -> f Char
-- example r = get #b r
-- :}
--
-- but it doesn't have to be:
--
-- >>> :{
-- example :: Record I (Merge '[ "a" := Bool ] r) -> I Bool
-- example = get #a
-- :}
--
-- However, just like in the case of unknown fields (see example in 'get'),
-- if earlier parts in the record are unknown we get type error:
--
-- >>> :{
-- example :: Record I (Merge r '[ "b" := Char ]) -> I Char
-- example r = get #b r
-- :}
-- ...
-- ...No instance for (RecordHasField "b"...
-- ...
merge :: Record f r -> Record f r' -> Record f (Merge r r')
merge = A.merge

-- | Project from one record to another
--
-- Both the source record and the target record must be fully known.
--
-- The target record can omit fields from the source record, as well as
-- rearrange them:
--
-- >>> :{
-- example ::
--      Record f [ "a" := Char, "b" := Int, "c" := Bool ]
--   -> Record f [ "c" := Bool, "a" := Char ]
-- example = project
-- :}
--
-- Of course, it is not possible to /add/ fields:
--
-- >>> :{
-- example ::
--      Record f [ "c" := Bool, "a" := Char ]
--   -> Record f [ "a" := Char, "b" := Int, "c" := Bool ]
-- example = project
-- :}
-- ...
-- ...No instance for (Project...
-- ...
--
-- Type inference will work through projections: field types are unified based
-- on their name:
--
-- >>> :{
-- example ::
--      Record f [ "a" := Char, "b" := Int, "c" := Bool ]
--   -> Record f [ "c" := _, "a" := Char ]
-- example = project
-- :}
-- ...
-- ...Found type wildcard...Bool...
-- ...
--
-- As we saw in 'merge', 'project' can also flatten 'Merge'd rows.
project :: Project f r r' => Record f r -> Record f r'
project = A.project

-- | Lens from one record to another
--
-- See 'project' for examples ('project' is just the lens getter, without the
-- setter).
lens ::
     Project f r r'
  => Record f r
  -> (Record f r', Record f r' -> Record f r)
lens = A.lens

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Analogue to 'Prelude.fmap'
map :: (forall x. f x -> g x) -> Record f r -> Record g r
map f = A.map f

-- | Analogue to 'Prelude.mapM'
mapM :: Monad m => (forall x. f x -> m (g x)) -> Record f r -> m (Record g r)
mapM f = A.mapM f

-- | Constrained form of 'map'
cmap ::
     AllFields r c
  => Proxy c -> (forall x. c x => f x -> g x) -> Record f r -> Record g r
cmap p f = A.cmap p f

-- | Constrained form of 'cmap'
cmapM ::
     (Monad m, AllFields r c)
  => Proxy c
  -> (forall x. c x => f x -> m (g x))
  -> Record f r -> m (Record g r)
cmapM p f = A.cmapM p f

-- | Analogue of 'Prelude.pure'
pure :: KnownFields r => (forall x. f x) -> Record f r
pure f = A.pure f

-- | Constrained form of 'pure'
cpure ::
     (KnownFields r, AllFields r c)
  => Proxy c -> (forall x. c x => f x) -> Record f r
cpure p f = A.cpure p f

-- | Analogue of '<*>'
ap :: Record (f -.-> g) r -> Record f r -> Record g r
ap = A.ap

-- | Analogue of 'Data.Foldable.toList'
collapse :: Record (K a) r -> [a]
collapse = A.collapse

-- | Like 'collapse', but also include field names
toList :: KnownFields r => Record (K a) r -> [(String, a)]
toList = A.toList

-- | Analogue of 'Prelude.sequenceA'
sequenceA :: Monad m => Record (m :.: f) r -> m (Record f r)
sequenceA = A.sequenceA

-- | Simplified form of 'sequenceA'
sequenceA' :: Monad m => Record m r -> m (Record I r)
sequenceA' = A.sequenceA'

-- | Analogue of 'Prelude.zip'
zip :: Record f r -> Record g r -> Record (Product f g) r
zip = A.zip

-- | Analogue of 'Prelude.zipWith'
zipWith ::
     (forall x. f x -> g x -> h x)
  -> Record f r -> Record g r -> Record h r
zipWith f = A.zipWith f

-- | Analogue of 'Control.Monad.zipWithM'
zipWithM ::
     Monad m
  => (forall x. f x -> g x -> m (h x))
  -> Record f r -> Record g r -> m (Record h r)
zipWithM f = A.zipWithM f

-- | Constrained form of 'zipWith'
czipWith ::
     AllFields r c
  => Proxy c
  -> (forall x. c x => f x -> g x -> h x)
  -> Record f r -> Record g r -> Record h r
czipWith p f = A.czipWith p f

-- | Constrained form of 'zipWithM'
czipWithM ::
     (Monad m, AllFields r c)
  => Proxy c
  -> (forall x. c x => f x -> g x -> m (h x))
  -> Record f r -> Record g r -> m (Record h r)
czipWithM p f = A.czipWithM p f

{-------------------------------------------------------------------------------
  Constraint reification and reflection
-------------------------------------------------------------------------------}

-- | Record of dictionaries
--
-- This reifies an 'AllFields' constraint as a record.
--
-- Inverse to 'reflectKnownFields'.
reifyAllFields :: AllFields r (Compose c f) => proxy c -> Record (Dict c :.: f) r
reifyAllFields = A.reifyAllFields

-- | Establish 'KnownFields' from a record of field names
--
-- Inverse to 'reifyAllFields'.
reflectKnownFields :: Record (K String) r -> Reflected (KnownFields r)
reflectKnownFields = A.reflectKnownFields

-- | Record of field names
--
-- This reifies a 'KnownFields' constraint as a record.
--
-- Inverse to 'reflectAllFields'.
reifyKnownFields :: KnownFields r => proxy r -> Record (K String) r
reifyKnownFields = A.reifyKnownFields

-- | Establish 'AllFields' from a record of dictionaries
--
-- Inverse to 'reifyKnownFields'.
reflectAllFields :: Record (Dict c) r -> Reflected (AllFields r c)
reflectAllFields = A.reflectAllFields

-- | Runtime check if we can project from one record to another
--
-- Since we cannot do runtime type checks, we insist that the fields of the
-- record must all be of one type @a@.
reflectProject ::
     (KnownFields r, KnownFields r')
  => proxy  r
  -> proxy' r'
  -> Either CannotProject (Reflected (Project (K a) r r'))
reflectProject = A.reflectProject

{-------------------------------------------------------------------------------
  Existential records
-------------------------------------------------------------------------------}

-- | Construct record with existentially quantified row variable
--
-- Existentially quantified records arise for example when parsing JSON values
-- as records. The typical process is as follows:
--
-- 1.  Construct a record with an existentially quantified row variable @r@ using
--     'someRecord'. Pattern match on the result to bring @r@ into scope.
--
-- 2.  In order to prove 'KnownFields' for @r@, pick an @f@ such that it's
--     possible to define a function
--
--     > fieldName :: f x -> K String x
--
--     'map' this function over the record, then use 'reflectKnownFields' to
--     prove 'KnownFields'.
--
-- 3.  At this point there are two alternatives:
--
--     a.  Use 'reflectAllFields' to prove that 'AllFields c' holds for your
--         record, for one or more constraints @c@. To be able to do this, you
--         will need to choose your @f@ such that (in addition to 'fieldName')
--         you can define
--
--         > fieldDict :: f x -> Dict (Compose c f) x
--
--         This option is primarily useful when dealing with records only
--         "wholesale", never accessing (statically known) fields of the record.
--
--     b.  Use 'reflectProject' to check whether there we can construct a lens
--         from the existentially quantified record to another record. Typically
--         the type of that other record /is/ statically known, which means that
--         once we have such a lens, further discovery of additional constraints
--         is not necesssary: these constraints can be proved in the normal way
--         for the concrete type.
someRecord :: [Some f] -> Some (Record f)
someRecord = A.someRecord

{-------------------------------------------------------------------------------
  Experimental integration with @typelet@
-------------------------------------------------------------------------------}

-- | Introduce type variable for a row
--
-- This can be used in conjunction with 'letInsertAs':
--
-- >>> :{
-- example :: Record I '[ "a" := Int, "b" := Char, "c" := Bool ]
-- example = letRecordT $ \p -> castEqual $
--     letInsertAs p #c (I True) empty $ \xs02 ->
--     letInsertAs p #b (I 'X' ) xs02  $ \xs01 ->
--     letInsertAs p #a (I 1   ) xs01  $ \xs00 ->
--     castEqual xs00
-- :}
letRecordT :: forall r f.
     (forall r'. Let r' r => Proxy r' -> Record f r)
  -> Record f r
letRecordT f = A.letRecordT f

-- | Insert field into a record and introduce type variable for the result
letInsertAs :: forall r r' f n a.
     Proxy r       -- ^ Type of the record we are constructing
  -> Field n       -- ^ New field to be inserted
  -> f a           -- ^ Value of the new field
  -> Record f r'   -- ^ Record constructed so far
  -> (forall r''. Let r'' (n := a : r') => Record f r'' -> Record f r)
                   -- ^ Assign type variable to new partial record, and continue
  -> Record f r
letInsertAs p n x r f = A.letInsertAs p n x r f



