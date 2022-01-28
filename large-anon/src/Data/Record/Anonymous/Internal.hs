{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Record.Anonymous.Internal (
    -- * Types
    Record -- Opaque
  , Field  -- Opaque
  , Merge
  , Isomorphic
    -- * User-visible API
  , empty
  , insert
  , merge
  , castRecord
    -- * Convenience functions
  , get
  , set
    -- * Combinators
    -- ** "Functor"
  , map
  , mapM
    -- ** Zipping
  , zip
  , zipWith
  , zipWithM
    -- ** "Foldable"
  , collapse
    -- ** "Traversable"
  , sequenceA
    -- ** "Applicative"
  , pure
  , ap
    -- * Generics
    -- ** Metadata
  , RecordMetadata -- opaque
  , recordMetadata
  , RecordMetadataOf
    -- ** Conversion
  , normalizeRecordRep
  , denormalizeRecordRep
  , recordToRep
  , recordToRep'
  , recordFromRep
  , recordFromRep'
  , recordToNormalizedRep
  , recordFromNormalizedRep
    -- ** Constraints
  , RecordDicts(..)
  , RecordDictsF
  , recordDictsF
  , RecordConstraints(..)
    -- * Internal API
  , unsafeRecordHasField
  , unsafeDictRecord
  , unsafeRecordDicts
  , unsafeRecordMetadata
  ) where

import Prelude hiding (map, mapM, zip, zipWith, sequenceA, pure)
import qualified Prelude

import Data.Aeson
import Data.Coerce (coerce)
import Data.Functor.Product
import Data.Kind
import Data.Map (Map)
import Data.Proxy
import Data.Record.Generic.Eq
import Data.Record.Generic.JSON
import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)
import Data.Record.Generic.Show
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.Records.Compat
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Foldable       as Foldable
import qualified Data.Map            as Map
import qualified Data.Map.Merge.Lazy as Map

import Data.Record.Generic

import qualified Data.Record.Generic.Rep.Internal as Rep

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedLabels
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeApplications
-- >>> :set -XTypeFamilies
-- >>> :set -fplugin=Data.Record.Anonymous.Plugin
-- >>> :set -dppr-cols=200
-- >>> import Data.SOP.BasicFunctors
-- >>> import GHC.Records.Compat
-- >>> let example :: Record I '[ '("a", Bool) ] = insert #a (I True) empty

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Anonymous record
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
-- TODO: Think about laziness/strictness (here and elsewhere)
newtype Record f (r :: [(Symbol, Type)]) = MkR (Map String (f Any))

type role Record nominal representational

data Field l where
  Field :: KnownSymbol l => Proxy l -> Field l

instance (l ~ l', KnownSymbol l) => IsLabel l' (Field l) where
  fromLabel = Field (Proxy @l)

-- | Merge two records
--
-- The 'Merge' type family does not reduce: the following two types are /not/
-- considered to be equal:
--
-- > Record '[ '("a", Bool) ']
-- > Record (Merge '[ '("a", Bool) '] '[])
--
-- They are /isomorphic/ (see 'castRecord'), but not /equal/.
--
-- As a consequence, the 'Merge' type family is injective: if
--
-- > Merge xs ys ~ Merge xs' ys'
--
-- then @xs ~ xs'@ and @ys ~ ys'@. Example:
--
-- >>> :{
--   let foo :: Merge '[ '("a", Bool) ] '[] ~ Merge '[] '[ '("a", Bool) ] => ()
--       foo = ()
--   in foo
-- :}
-- ...
-- ...Couldn't match...[]...
-- ...
--
-- See 'merge' for additional information.
type family Merge :: [(Symbol, Type)] -> [(Symbol, Type)] -> [(Symbol, Type)]

-- | Record isomorphism
--
-- See 'castRecord' for details.
class Isomorphic (xs :: [(Symbol, Type)]) (ys :: [(Symbol, Type)])

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

recordToMap :: Record f r -> Map String (f Any)
recordToMap (MkR r) = r

recordFromMap :: Map String (f Any) -> Record f r
recordFromMap = MkR

{-------------------------------------------------------------------------------
  User-visible API
-------------------------------------------------------------------------------}

-- | Empty record
empty :: Record f '[]
empty = MkR Map.empty

-- | Insert a new field into a record
--
-- If a field with this name already exists, the new field will override it.
insert :: Field l -> f a -> Record f r -> Record f ('(l, a) ': r)
insert (Field l) a (MkR r) = MkR $ Map.insert (symbolVal l) (co a) r
  where
    co :: f a -> f Any
    co = noInlineUnsafeCo

-- | Merge two records
--
-- 'HasField' constraint can be resolved for merged records, subject to the same
-- condition discussed in the documentation of 'Record': since records are left
-- biased, all fields in the record must be known up to the requested field:
--
-- Simple example, completely known record:
--
-- >>> :{
--   let example :: Record I (Merge '[ '("a", Bool)] '[ '("b", Char)])
--       example = merge (insert #a (I True) empty) (insert #b (I 'a') empty)
--   in get #b example
-- :}
-- I 'a'
--
-- Slightly more sophisticated, only part of the record known:
--
-- >>> :{
--   let example :: Record I (Merge '[ '("a", Bool)] r) -> I Bool
--       example = get #a
--   in example (merge (insert #a (I True) empty) (insert #b (I 'a') empty))
-- :}
-- I True
--
-- Rejected example: first part of the record unknown:
--
-- >>> :{
--   let example :: Record I (Merge r '[ '("b", Char)]) -> I Char
--       example = get #b
--   in example (merge (insert #a (I True) empty) (insert #b (I 'a') empty))
-- :}
-- ...
-- ...No instance for (HasField "b" (...
-- ...
merge :: Record f r -> Record f r' -> Record f (Merge r r')
merge (MkR r) (MkR r') = MkR $ Map.union r r'

-- | Cast record
--
-- Some examples of valid casts. We can cast a record to itself:
--
-- >>> castRecord example :: Record I '[ '("a", Bool) ]
-- Record {a = I True}
--
-- We can reorder fields:
--
-- >>> castRecord (insert #a (I True) $ insert #b (I 'a') $ empty) :: Record I '[ '("b", Char), '("a", Bool) ]
-- Record {b = I 'a', a = I True}
--
-- We can flatten merged records:
--
-- >>> castRecord (merge (insert #a (I True) empty) (insert #b (I 'a') empty)) :: Record I '[ '("a", Bool), '("b", Char) ]
-- Record {a = I True, b = I 'a'}
--
-- Some examples of invalid casts. We cannot change the types of the fields:
--
-- >>> castRecord example :: Record I '[ '("a", Int) ]
-- ...
-- ...Couldn't match...Bool...Int...
-- ...
--
-- We cannot drop fields:
--
-- >>> castRecord (insert #a (I True) $ insert #b (I 'a') $ empty) :: Record I '[ '("a", Bool) ]
-- ...
-- ...No instance for (Isomorphic...
-- ...
--
-- We cannot add fields:
--
-- >>> castRecord example :: Record I '[ '("a", Bool), '("b", Char) ]
-- ...
-- ...No instance for (Isomorphic...
-- ...
castRecord :: Isomorphic r r' => Record f r -> Record f r'
castRecord = co
  where
    co :: Record f r -> Record f r'
    co = noInlineUnsafeCo

{-------------------------------------------------------------------------------
  Convenience functions

  Defined here for the sake of the docspec examples (these do not actually
  rely on internal API).
-------------------------------------------------------------------------------}

-- | Get record field
--
-- This is a simple wrapper for 'getField'.
get :: forall l f r a.
     HasField l (Record f r) (f a)
  => Field l -> Record f r -> f a
get _ = getField @l @(Record f r)

-- | Set record field
--
-- This is a simple wrapper for 'setField'.
set :: forall l f r a.
     HasField l (Record f r) (f a)
  => Field l -> f a -> Record f r -> Record f r
set _ = flip (setField @l @(Record f r))

{-------------------------------------------------------------------------------
  Simple (non-constrained) combinators
-------------------------------------------------------------------------------}

map :: (forall x. f x -> g x) -> Record f r -> Record g r
map f = recordFromMap . fmap f . recordToMap

mapM ::
     Applicative m
  => (forall x. f x -> m (g x))
  -> Record f r -> m (Record g r)
mapM f = fmap recordFromMap . traverse f . recordToMap

zip :: Record f r -> Record g r -> Record (Product f g) r
zip = zipWith Pair

zipWith ::
     (forall x. f x -> g x -> h x)
  -> Record f r -> Record g r -> Record h r
zipWith f a b = recordFromMap $
    Map.intersectionWith f (recordToMap a) (recordToMap b)

zipWithM ::
     Applicative m
  => (forall x. f x -> g x -> m (h x))
  -> Record f r -> Record g r -> m (Record h r)
zipWithM f a b = fmap recordFromMap $
    Map.mergeA
      Map.dropMissing -- can't actually happen
      Map.dropMissing -- can't actually happen
      (Map.zipWithAMatched $ const f)
      (recordToMap a)
      (recordToMap b)

collapse :: Record (K a) r -> [a]
collapse = co . Foldable.toList . recordToMap
  where
    co :: [K a Any] -> [a]
    co = coerce

sequenceA :: Applicative m => Record (m :.: f) r -> m (Record f r)
sequenceA = fmap recordFromMap . traverse unComp . recordToMap

pure :: forall f r. RecordMetadata r => (forall x. f x) -> Record f r
pure f = recordFromMap $ aux recordMetadata
  where
    aux :: Metadata (Record f r) -> Map String (f Any)
    aux =
          Map.fromList
        . Prelude.map (($ f) . (,))
        . Rep.collapse
        . recordFieldNames

ap :: Record (f -.-> g) r -> Record f r -> Record g r
ap fs as = recordFromMap $
    Map.intersectionWith apFn (recordToMap fs) (recordToMap as)

{-------------------------------------------------------------------------------
  Constraints

  [Note RecordDicts]

  The constrained combinators on 'Record' are defined in terms of 'RecordDicts',
  not 'RecordDictsF'. This is a non-trivial design choice which requires some
  justification:

  1. More primitive

  Consider 'cpure' versus @cpureF@ (hypothetical, not offered by the lib):

  > cpure  :: RecordDicts    r c => Proxy c -> (forall x. c    x  => f x) -> Record f r
  > cpureF :: RecordDictsF f r c => Proxy c -> (forall x. c (f x) => f x) -> Record f r

  At first glance, @cpureF@ might seem like the more logical choice; after all,
  each field in the record has type @f x@ for some @x@. However, @cpureF@ is
  easily defined in terms of 'cpure':

  > cpureF _p f = cpure (Proxy @(Compose c f)) f

  but we /cannot/ define `cpure` in terms of `cpureF`. We would have to pick
  some functor @f@; we would like to choose the identify type family, but we
  cannot (cannot partially apply type families). If instead we choose the
  identity newtype, we have no guarantee that @c (I x)@ implies @c x@.

  2. More natural

  Consider 'cmap':

  > cmap ::
  >      RecordDicts r c
  >   => Proxy c
  >   -> (forall x. c x => f x -> g x)
  >   -> Record f r -> Record g r
  > cmap p f = ap $ cpure p (Fn f)

  If we wanted to define this in terms of 'RecordDictsF', what should the
  type of the callback be? Any of the following could be useful in different
  circumstances:

  > cmapF :: ... -> (forall x. (c (f x)         ) => f x -> g x) -> ...
  > cmapF :: ... -> (forall x.           c (g x)) => f x -> g x) -> ...
  > cmapF :: ... -> (forall x. (c (f x), c (g x)) => f x -> g x) -> ...

  None of these is more general then the other (the third version is the most
  general from the point of view of the callback, but the most restrictive
  from the point of view of the caller of 'cmap').

  Additionally, currently 'cmap' is defined in terms of 'cpure':

  > cmap p f = ap (cpure p (Fn f) :: Record (f -.-> g) r)

  It is not all obvious how we could define @cmapF@ (any of the three variants)
  in terms of @cpureF@; we could probably define it directly in terms of
  'recordDictsF' (like @cpureF@ itself), but now we are losing compositionality.

  3. More in line with the combinators on 'Rep'

  Compare

  > cpure :: (Generic a, Constraints a c) => Proxy c -> (forall x. c x => f x) -> Rep    f a
  > cpure ::             RecordDicts r c  => Proxy c -> (forall x. c x => f x) -> Record f r

  The @large-generics@ infrastructure is defined over types of kind @Type@, not
  over types of kind @(Type -> Type) -> Type@, and so the 'Constraints' /cannot/
  include a functor argument. That doesn't mean of course that 'cpure' could not
  be defined as

  > cpure :: (Generic a, Constraints a (Compose c f)) => ..

  but in the context of @large-generics@ the functor-less version is very
  natural (and works well). That doesn't necessarily mean 'cpure' for 'Record'
  should suit, of course, but it's nice that they do line up.
-------------------------------------------------------------------------------}

-- | Require @c a@ for every field of type @a@ in a @Record f r@
--
-- This does /not/ deal with the functor argument @f@ (see 'RecordDictsF').
-- For a discussion on why 'RecordDicts' is primitive, see [Note RecordDicts].
class RecordDicts r c where
  recordDicts :: Proxy c -> Record (Dict c) r

-- TODO: Provide way to go the other direction.

-- | Require @c (f a)@ for every field of type @a@ in a @Record f r@
class    RecordDicts r (Compose c f) => RecordDictsF f r c
instance RecordDicts r (Compose c f) => RecordDictsF f r c

recordDictsF :: forall c f r.
     RecordDictsF f r c
  => Proxy c -> Record (Dict c :.: f) r
recordDictsF _ = map aux $ recordDicts (Proxy @(Compose c f))
  where
    aux :: Dict (Compose c f) x -> (Dict c :.: f) x
    aux Dict = Comp Dict

{-------------------------------------------------------------------------------
  Generics: Metadata
-------------------------------------------------------------------------------}

-- | For the record metadata, the functor used for the 'Record' is irrelevant
data Irrelevant (a :: Type)

class RecordMetadata (r :: [(Symbol, Type)]) where
  recordMetadata' :: Metadata (Record Irrelevant r)

recordMetadata :: RecordMetadata r => Metadata (Record f r)
recordMetadata = co recordMetadata'
  where
    co :: Metadata (Record Irrelevant r) -> Metadata (Record f r)
    co = noInlineUnsafeCo

{-------------------------------------------------------------------------------
  Generics: Conversion to/from 'Rep'

  It is unfortunate these these translations are @O(n log n)@. It is not obvious
  however how to address this without making other operations more expensive.
  In particular, we could decide the internal representation of a record to /be/
  a vector, but if we do, 'insert' suddenly becomes @O(n)@, which would make a
  repeated series of inserts (a common pattern) @O(n²)@. For now we just accept
  the cost here: the cost is only incurred when translating to/from the generic
  representation, and it's probably not a good idea to use generic code in
  performance critical code anyway.

  Both 'recordToRep' and 'recordFromRep' need the metadata:

  - 'recordToRep' needs the metadata to determine the right order of the fields
  - 'recordFromRep' needs the metadata for the keys in the 'Map'
-------------------------------------------------------------------------------}

-- | Push functor argument into 'Rep', so it can be operated on
--
-- The name is by analogy to @normalize@ in "Data.Record.Generic.Transform",
-- although the details are different. In particular, 'normalizeRecordRep'
-- is always safe, we do not need the equivalent of @HasNormalForm d x y@.
normalizeRecordRep :: Rep I (Record f r) -> Rep f (Record I r)
normalizeRecordRep = unsafeCoerce

-- | Inverse to 'normalizeRecordRep'
denormalizeRecordRep :: Rep f (Record I r) -> Rep I (Record f r)
denormalizeRecordRep = unsafeCoerce

-- | Generalization of 'recordToRep'
recordToRep' :: forall f g r.
     RecordMetadata r
  => Record (f :.: g) r -> Rep f (Record g r)
recordToRep' (MkR r) =
    Rep.unsafeFromListAny $ Prelude.map aux names
  where
    names :: [String]
    names = Rep.collapse $ recordFieldNames $ metadata (Proxy @(Record f r))

    aux :: String -> f Any
    aux nm = case Map.lookup nm r of
               Just x  -> co x
               Nothing -> error "impossible: non-existent field"

    co :: (f :.: g) Any -> f Any
    co = noInlineUnsafeCo

-- | Generalization of 'recordFromRep'
recordFromRep' :: forall f g r.
     RecordMetadata r
  => Rep f (Record g r) -> Record (f :.: g) r
recordFromRep' =
    MkR . Map.fromList . Prelude.zipWith aux names . Rep.toListAny
  where
    names :: [String]
    names = Rep.collapse $ recordFieldNames $ metadata (Proxy @(Record f r))

    aux :: String -> f Any -> (String, (f :.: g) Any)
    aux name x = (name, co x)

    co :: f Any -> (f :.: g) Any
    co = noInlineUnsafeCo

-- | Convert @Record f@ to @Rep I@
recordToRep :: forall f r.
     RecordMetadata r
  => Record f r -> Rep I (Record f r)
recordToRep = recordToRep' . co
  where
    co :: Record f r -> Record (I :.: f) r
    co = noInlineUnsafeCo

-- | Convert @Rep I@ to @Record f@
recordFromRep :: forall f r.
     RecordMetadata r
  => Rep I (Record f r) -> Record f r
recordFromRep = co . recordFromRep'
  where
    co :: Record (I :.: f) r -> Record f r
    co = noInlineUnsafeCo

-- | Convert @Record f@ to @Rep f@
recordToNormalizedRep :: forall f r.
     RecordMetadata r
  => Record f r -> Rep f (Record I r)
recordToNormalizedRep = normalizeRecordRep . recordToRep

-- | Convert @Rep f@ to @Record f@
recordFromNormalizedRep :: forall f r.
     RecordMetadata r
  => Rep f (Record I r) -> Record f r
recordFromNormalizedRep = recordFromRep . denormalizeRecordRep

{-------------------------------------------------------------------------------
  Generics
-------------------------------------------------------------------------------}

class RecordMetadata r => RecordConstraints f r c where
  dictRecord :: Proxy c -> Rep (Dict c) (Record f r)

type family RecordMetadataOf (f :: Type -> Type) (r :: [(Symbol, Type)]) :: [(Symbol, Type)]
  -- Rewritten by the plugin

instance RecordMetadata r => Generic (Record f r) where
  type Constraints (Record f r) = RecordConstraints f r -- RecordDictsF f r
  type MetadataOf  (Record f r) = RecordMetadataOf  f r

  metadata _ = recordMetadata
  from       = recordToRep
  to         = recordFromRep
  dict       = dictRecord      -- recordToRep' . recordDictsF

{-------------------------------------------------------------------------------
  Instances

  These could be defined outside of the @Internal@ module: we do not need access
  to the low-level representation. We define them here anyway for two reasons:

  1. Avoid orphans
  2. The doctest examples in this module rely on them.
-------------------------------------------------------------------------------}

instance RecordConstraints f r Show => Show (Record f r) where
  showsPrec = gshowsPrec

instance RecordConstraints f r Eq => Eq (Record f r) where
  (==) = geq

instance ( RecordConstraints f r Eq
         , RecordConstraints f r Ord
         ) => Ord (Record f r) where
  compare = gcompare

instance RecordConstraints f r ToJSON => ToJSON (Record f r) where
  toJSON = gtoJSON

instance RecordConstraints f r FromJSON => FromJSON (Record f r) where
  parseJSON = gparseJSON

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

-- | Used by the plugin to construct evidence for 'HasField'
--
-- Precondition: the record must have the specified field with type @a@, (where
-- @a@ will be of the form @f a'@ for some @a'). This precondition is verified
-- by the plugin before generating "evidence" that uses this function.
unsafeRecordHasField :: forall f r a.
     String
  -> Record f r
  -> (a -> Record f r, a)
unsafeRecordHasField label (MkR r) = (
      \a -> MkR $ Map.insert label (co a) r
    , case Map.lookup label r of
        Just f  -> co' f
        Nothing -> error preconditionViolation
    )
  where
    preconditionViolation :: String
    preconditionViolation = concat [
          "unsafeRecordHasField precondition violation: field "
        , label
        , " not found"
        ]

    co  :: a -> f Any
    co' :: f Any -> a

    co  = noInlineUnsafeCo
    co' = noInlineUnsafeCo

-- | Used by the plugin to construct evidence for 'RecordConstraints'
--
-- TODO: Delete
unsafeDictRecord :: forall f r c.
     [Dict c (f Any)]  -- ^ Dictionary for each field, in order
  -> Proxy c
  -> Rep (Dict c) (Record f r)
unsafeDictRecord ds _ = Rep.unsafeFromListAny (Prelude.map co ds)
  where
    co :: Dict c (f Any) -> Dict c Any
    co = noInlineUnsafeCo

-- | Used by the plugin to construct evidence for 'RecordDicts'
--
-- Precondition: the list must have an entry for each field in the record,
-- with an appropriate dictionary for that field.
unsafeRecordDicts :: forall c r.
     [(String, Dict c Any)]
  -> Proxy c
  -> Record (Dict c) r
unsafeRecordDicts dicts _ = MkR $ Map.fromList dicts

-- | Used by the plugin to construct evidence for 'RecordMetadata'
unsafeRecordMetadata :: forall r.
     [FieldMetadata Any]
  -> Metadata (Record Irrelevant r)
unsafeRecordMetadata fields = Metadata {
      recordName          = "Record"
    , recordConstructor   = "Record"
    , recordSize          = length fields
    , recordFieldMetadata = Rep.unsafeFromListAny fields
    }

