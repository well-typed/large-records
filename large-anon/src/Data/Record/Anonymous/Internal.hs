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
  , RecordConstraints(..)
  , RecordMetadata(..)
  , RecordMetadataOf
    -- * Internal API
  , unsafeRecordHasField
  , unsafeDictRecord
  , unsafeFieldMetadata
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
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.Records.Compat
import GHC.TypeLits

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

-- | Internal function
recordToMap :: Record f r -> Map String (f Any)
recordToMap (MkR r) = r

-- | Internal function
recordFromMap :: Map String (f Any) -> Record f r
recordFromMap = MkR

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

pure :: forall f r. RecordMetadata f r => (forall x. f x) -> Record f r
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
  Generics
-------------------------------------------------------------------------------}

class RecordMetadata (f :: Type -> Type) (r :: [(Symbol, Type)]) where
  recordMetadata :: Metadata (Record f r)

class RecordMetadata f r => RecordConstraints f r c where
  dictRecord :: Proxy c -> Rep (Dict c) (Record f r)

type family RecordMetadataOf (f :: Type -> Type) (r :: [(Symbol, Type)]) :: [(Symbol, Type)]
  -- Rewritten by the plugin

instance RecordMetadata f r => Generic (Record f r) where
  type Constraints (Record f r) = RecordConstraints f r
  type MetadataOf  (Record f r) = RecordMetadataOf  f r

  dict       = dictRecord
  metadata _ = recordMetadata

  -- Implementation note: the order of the fields in the vector must match
  -- the order as specified by the user in the type.
  --
  -- TODO: Can we avoid the O(n log n) cost?

  from :: Record f r -> Rep I (Record f r)
  from (MkR r) =
      Rep.unsafeFromListAny $ Prelude.map aux names
    where
      names :: [String]
      names = Rep.collapse $ recordFieldNames $ metadata (Proxy @(Record f r))

      aux :: String -> I Any
      aux nm = case Map.lookup nm r of
                 Just x  -> I (co x)
                 Nothing -> error "impossible: non-existent field"

      co :: f Any -> Any
      co = noInlineUnsafeCo

  to :: Rep I (Record f r) -> Record f r
  to =
      MkR . Map.fromList . Prelude.zipWith aux names . Rep.toListAny
    where
      names :: [String]
      names = Rep.collapse $ recordFieldNames $ metadata (Proxy @(Record f r))

      aux :: String -> I Any -> (String, f Any)
      aux name (I x) = (name, co x)

      co :: Any -> f Any
      co = noInlineUnsafeCo

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

-- | Used by the plugin during evidence construction for 'HasField'
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

-- | Used by the plugin during evidence construction for 'RecordConstraints'
unsafeDictRecord :: forall f r c.
     [Dict c (f Any)]  -- ^ Dictionary for each field, in order
  -> Proxy c
  -> Rep (Dict c) (Record f r)
unsafeDictRecord ds _ = Rep.unsafeFromListAny (Prelude.map co ds)
  where
    co :: Dict c (f Any) -> Dict c Any
    co = noInlineUnsafeCo

-- | Used by the plugin during evidence construction for 'RecordMetadata'
unsafeFieldMetadata :: forall f r.
     [FieldMetadata (f Any)]
  -> Rep FieldMetadata (Record f r)
unsafeFieldMetadata = Rep.unsafeFromListAny . Prelude.map co
  where
    co :: FieldMetadata (f Any) -> FieldMetadata Any
    co = noInlineUnsafeCo
