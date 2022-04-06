{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Full record representation
--
-- Intended for qualified import.
--
-- > import Data.Record.Anon.Internal.Advanced (Record)
-- > import qualified Data.Record.Anon.Internal.Advanced as A
module Data.Record.Anon.Internal.Advanced (
    -- * Definition
    Record -- opaque
    -- * Main API
  , Field(..)
  , empty
  , insert
  , insertA
  , get
  , set
  , merge
  , lens
  , project
  , inject
  , applyPending
    -- * Combinators
    -- ** " Foldable "
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
    -- * Reification and reflection
  , reifyKnownFields
  , reflectKnownFields
  , reifyAllFields
  , reflectAllFields
  , InRow(..)
  , reifyProject
  , reflectProject
    -- * Existential records
  , Some(..)
  , SomeRecord(..)
  , someRecord
    -- * Support for @typelet@
  , letRecordT
  , letInsertAs
  ) where

import Prelude hiding (map, mapM, zip, zipWith, sequenceA, pure)
import qualified Prelude

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Functor.Product
import Data.Kind
import Data.Primitive.SmallArray
import Data.Proxy
import Data.Record.Generic hiding (FieldName)
import Data.SOP.Classes (fn_2)
import Data.SOP.Constraint
import Data.Tagged
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.Records.Compat
import GHC.TypeLits
import TypeLet.UserAPI

import qualified Optics.Core as Optics

import qualified Data.Record.Generic.Eq   as Generic
import qualified Data.Record.Generic.JSON as Generic
import qualified Data.Record.Generic.Show as Generic

import Data.Record.Anon.Internal.Core.Canonical (Canonical)
import Data.Record.Anon.Internal.Core.Diff (Diff)
import Data.Record.Anon.Internal.Core.FieldName
import Data.Record.Anon.Internal.Core.Util.StrictVector (StrictVector)
import Data.Record.Anon.Internal.Reflection (Reflected(..))
import Data.Record.Anon.Plugin.Internal.Runtime

import qualified Data.Record.Anon.Internal.Core.Canonical as Canon
import qualified Data.Record.Anon.Internal.Core.Diff      as Diff
import qualified Data.Record.Anon.Internal.Reflection     as Unsafe
import qualified Data.Record.Anon.Internal.Core.Util.StrictVector as Strict

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Anonymous record
data Record (f :: k -> Type) (r :: Row k) =
    NoPending  {-# UNPACK #-} !(Canonical f)
  | HasPending {-# UNPACK #-} !(Canonical f) !(Diff f)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Construct canonical form of the record (i.e., apply the internal 'Diff')
--
-- This is @O(n)@, and should be done only for operations on records that are
-- @O(n)@ /anyway/, so that the cost can be absorbed.
toCanonical :: Record f r -> Canonical f
toCanonical (NoPending  c)   = c
toCanonical (HasPending c d) = Diff.apply d c

-- | Construct 'Record' from 'Canonical' representation (empty 'Diff')
--
-- This function is unsafe because we cannot verify whether the record matches
-- it's row specification @r@.
unsafeFromCanonical :: Canonical f -> Record f r
unsafeFromCanonical = NoPending

{-------------------------------------------------------------------------------
  HasField
-------------------------------------------------------------------------------}

-- | Proxy for a field name, with 'IsLabel' instance
--
-- The 'IsLabel' instance makes it possible to write
--
-- > #foo
--
-- to mean
--
-- > Field (Proxy @"foo")
data Field n where
  Field :: (KnownSymbol n, KnownHash n) => Proxy n -> Field n

instance (n ~ n', KnownSymbol n, KnownHash n) => IsLabel n' (Field n) where
  fromLabel = Field (Proxy @n)

instance forall k (n :: Symbol) (f :: k -> Type) (r :: Row k) (a :: k).
       (KnownSymbol n, KnownHash n, RowHasField n r a)
    => HasField n (Record f r) (f a) where

  -- INLINE pragma important: it makes the 'NoPendingCases' cases very close
  -- to the performance of using a 'SmallArray' directly.
  {-# INLINE hasField #-}
  hasField r = (
        \x -> unsafeSetField ix name x r
      , unsafeGetField ix name r
      )
    where
      name :: FieldName
      name = mkFieldName (Proxy @n)

      ix :: Int
      ix = proxy rowHasField (Proxy @'(n, r, a))

-- | Compile-time construction of a 'FieldName'
mkFieldName :: (KnownSymbol n, KnownHash n) => Proxy n -> FieldName
mkFieldName p = FieldName (hashVal p) (symbolVal p)

instance (RowHasField n r a, KnownSymbol n, KnownHash n)
      => Optics.LabelOptic n Optics.A_Lens (Record f r) (Record f r) (f a) (f a) where
  labelOptic = aux (fromLabel @n)
    where
      aux :: Field n -> Optics.Lens' (Record f r) (f a)
      aux n = Optics.lens (get n) (flip (set n))

-- | Low level field accessor
--
-- It is the responsibility of the plugin to ensure that the field index and
-- the field type match.
unsafeGetField :: forall k (f :: k -> Type) (r :: Row k) (a :: k).
    Int -> FieldName -> Record f r -> f a
unsafeGetField i n = co . \case
    NoPending  c   -> Canon.getAtIndex c i
    HasPending c d -> Diff.get (i, n) d c
  where
    co  :: f Any -> f a
    co = noInlineUnsafeCo

-- | Low level field update
--
-- See comments in 'getField'.
unsafeSetField :: forall k (f :: k -> Type) (r :: Row k) (a :: k).
    Int -> FieldName -> f a -> Record f r -> Record f r
unsafeSetField i n x = \case
    NoPending  c   -> HasPending c (go Diff.empty)
    HasPending c d -> HasPending c (go d)
  where
    go :: Diff f -> Diff f
    go = Diff.set (i, n) (co x)

    co :: f a -> f Any
    co = noInlineUnsafeCo

get :: forall n f r a.
     RowHasField n r a
  => Field n -> Record f r -> f a
get (Field _) = getField @n @(Record f r)

set :: forall n f r a.
     RowHasField n r a
  => Field n -> f a -> Record f r -> Record f r
set (Field _) = flip (setField @n @(Record f r))

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

empty :: Record f '[]
empty = NoPending mempty

insert :: forall k (f :: k -> Type) (r :: Row k) (a :: k) (n :: Symbol).
    Field n -> f a -> Record f r -> Record f (n := a : r)
insert (Field n) x = \case
    NoPending  c   -> HasPending c (go Diff.empty)
    HasPending c d -> HasPending c (go d)
  where
    go :: Diff f -> Diff f
    go = Diff.insert (mkFieldName n) (co x)

    co :: f a -> f Any
    co = noInlineUnsafeCo

insertA ::
     Applicative m
  => Field n -> m (f a) -> m (Record f r) -> m (Record f (n := a : r))
insertA f x r = insert f <$> x <*> r

merge :: Record f r -> Record f r' -> Record f (Merge r r')
merge (toCanonical -> r) (toCanonical -> r') =
    unsafeFromCanonical $ r <> r'

lens :: forall f r r'.
     Project r r'
  => Record f r -> (Record f r', Record f r' -> Record f r)
lens = \(toCanonical -> r) ->
    bimap getter setter $
      Canon.lens (proxy projectIndices (Proxy @'(r, r'))) r
  where
    getter :: Canonical f -> Record f r'
    getter = unsafeFromCanonical

    setter :: (Canonical f -> Canonical f) -> Record f r' -> Record f r
    setter f (toCanonical -> r) = unsafeFromCanonical (f r)

-- | Project out subrecord
--
-- This is just the 'lens' getter.
project :: Project r r' => Record f r -> Record f r'
project = fst . lens

-- | Inject subrecord
--
-- This is just the 'lens' setter.
inject :: Project r r' => Record f r' -> Record f r -> Record f r
inject small = ($ small) . snd . lens

applyPending :: Record f r -> Record f r
applyPending (toCanonical -> r) = unsafeFromCanonical r

{-------------------------------------------------------------------------------
  Unconstrained combinators
-------------------------------------------------------------------------------}

map :: (forall x. f x -> g x) -> Record f r -> Record g r
map f (toCanonical -> r) = unsafeFromCanonical $
    Canon.map f r

mapM ::
     Applicative m
  => (forall x. f x -> m (g x))
  -> Record f r -> m (Record g r)
mapM f (toCanonical -> r) = fmap unsafeFromCanonical $
    Canon.mapM f r

zip :: Record f r -> Record g r -> Record (Product f g) r
zip = zipWith Pair

zipWith ::
     (forall x. f x -> g x -> h x)
  -> Record f r -> Record g r -> Record h r
zipWith f (toCanonical -> r) (toCanonical -> r') = unsafeFromCanonical $
    Canon.zipWith f r r'

zipWithM ::
     Applicative m
  => (forall x. f x -> g x -> m (h x))
  -> Record f r -> Record g r -> m (Record h r)
zipWithM f (toCanonical -> r) (toCanonical -> r') = fmap unsafeFromCanonical $
    Canon.zipWithM f r r'

collapse :: Record (K a) r -> [a]
collapse (toCanonical -> r) =
    Canon.collapse r

sequenceA :: Applicative m => Record (m :.: f) r -> m (Record f r)
sequenceA (toCanonical -> r) = fmap unsafeFromCanonical $
    Canon.sequenceA r

sequenceA' :: Applicative m => Record m r -> m (Record I r)
sequenceA' = sequenceA . co
  where
    co :: Record m r -> Record (m :.: I) r
    co = noInlineUnsafeCo

pure :: forall f r. KnownFields r => (forall x. f x) -> Record f r
pure f = unsafeFromCanonical $
    Canon.fromList $ Prelude.map (const f) (proxy fieldNames (Proxy @r))

ap :: Record (f -.-> g) r -> Record f r -> Record g r
ap (toCanonical -> r) (toCanonical -> r') = unsafeFromCanonical $
    Canon.ap r r'

{-------------------------------------------------------------------------------
  Reification and reflection

  The @KnownFields@ constraint on @reifyProject@ is a little dissatisfying, as
  it feels like an orthogonal concern. Ultimately the reason is that in

  > Record f (r :: Row k)

  we have @f :: k -> Type@, as opposed to @f :: Symbol -> k -> Type@. That is
  a generalization we could at some point consider, but until we do, the

  > RowHasField n r a

  constraint introduced in the body 'InRow' involves an /existential/ @n@;
  a /separate/ record with 'KnownSymbol' evidence would therefore not give us
  any information about /this/ @n@.
-------------------------------------------------------------------------------}

reifyKnownFields :: forall k (r :: Row k) proxy.
     KnownFields r
  => proxy r -> Record (K String) r
reifyKnownFields _ =
    unsafeFromCanonical $
      Canon.fromList $ co $ proxy fieldNames (Proxy @r)
  where
    co :: [String] -> [K String Any]
    co = coerce

reflectKnownFields :: forall k (r :: Row k).
     Record (K String) r
  -> Reflected (KnownFields r)
reflectKnownFields names =
    Unsafe.reflectKnownFields $ Tagged $ collapse names

reifyAllFields :: forall k (r :: Row k) (c :: k -> Constraint) proxy.
     AllFields r c
  => proxy c -> Record (Dict c) r
reifyAllFields _ = unsafeFromCanonical $
    Canon.fromVector . Strict.fromLazy $
      fmap aux $ proxy fieldDicts (Proxy @r)
  where
    aux :: DictAny c -> Dict c Any
    aux DictAny = Dict

reflectAllFields :: forall k (c :: k -> Constraint) (r :: Row k).
     Record (Dict c) r
  -> Reflected (AllFields r c)
reflectAllFields dicts =
    Unsafe.reflectAllFields $ Tagged $
      fmap aux $ Strict.toLazy $ Canon.toVector $ toCanonical dicts
  where
    aux :: Dict c Any -> DictAny c
    aux Dict = DictAny

-- | @InRow r a@ is evidence that there exists some @n@ s.t. @(n := a)@ in @r@.
data InRow (r :: Row k) (a :: k) where
  InRow :: forall k (n :: Symbol) (r :: Row k) (a :: k).
       ( KnownSymbol n
       , RowHasField n r a
       )
    => Proxy n -> InRow r a

reifyProject :: forall k (r :: Row k) (r' :: Row k).
     (Project r r', KnownFields r')
  => Record (InRow r) r'
reifyProject =
    zipWith aux ixs (reifyKnownFields (Proxy @r'))
  where
    ixs :: Record (K Int) r'
    ixs = unsafeFromCanonical $
            Canon.fromVector $ co $ proxy projectIndices (Proxy @'(r, r'))

    co :: StrictVector Int -> StrictVector (K Int Any)
    co = coerce

    aux :: forall x. K Int x -> K String x -> InRow r x
    aux (K i) (K name) =
        case someSymbolVal name of
          SomeSymbol p -> unsafeInRow i p

reflectProject :: forall k (r :: Row k) (r' :: Row k).
     Record (InRow r) r'
  -> Reflected (Project r r')
reflectProject (toCanonical -> ixs) =
    Unsafe.reflectProject $ Tagged $
      (\inRow@(InRow p) -> aux inRow p) <$> Canon.toVector ixs
  where
    aux :: forall x n. RowHasField n r x => InRow r x -> Proxy n -> Int
    aux _ _ = proxy rowHasField (Proxy @'(n, r, x))

unsafeInRow :: forall n r a. KnownSymbol n => Int -> Proxy n -> InRow r a
unsafeInRow i p =
    case reflected of
      Reflected -> InRow p
  where
    reflected :: Reflected (RowHasField n r a)
    reflected = Unsafe.reflectRowHasField $ Tagged i

{-------------------------------------------------------------------------------
  Existential records
-------------------------------------------------------------------------------}

-- | Existential type ("there exists an @x@ such that @f x@")
data Some (f :: k -> Type) where
  Some :: forall k (f :: k -> Type) (x :: k). f x -> Some f

-- | Discovered row variable
--
-- See 'Data.Record.Anon.Advanced.someRecord' for detailed discussion.
data SomeRecord (f :: k -> Type) where
  SomeRecord :: forall k (r :: Row k) (f :: k -> Type).
       KnownFields r
    => Record (Product (InRow r) f) r
    -> SomeRecord f

someRecord :: forall k (f :: k -> Type). [(String, Some f)] -> SomeRecord f
someRecord fields =
    mkSomeRecord $
      unsafeFromCanonical . Canon.fromList $
        Prelude.zipWith aux [0..] (Prelude.map (first someSymbolVal) fields)
  where
    aux :: Int -> (SomeSymbol, Some f) -> Product (InRow r) f Any
    aux i (SomeSymbol n, Some fx) = Pair (unsafeInRow i n) (co fx)

    co :: f x -> f Any
    co = noInlineUnsafeCo

    mkSomeRecord :: forall r. Record (Product (InRow r) f) r -> SomeRecord f
    mkSomeRecord r =
        case reflected of
          Reflected -> SomeRecord r
      where
        reflected :: Reflected (KnownFields r)
        reflected = reflectKnownFields $ map getName r

        getName :: Product (InRow r) f x -> K String x
        getName (Pair (InRow p) _) = K $ symbolVal p

{-------------------------------------------------------------------------------
  Conversion to/from generic 'Rep'
-------------------------------------------------------------------------------}

recordToRep :: Record f r -> Rep I (Record f r)
recordToRep (toCanonical -> r) =
    Rep $ co . Strict.toLazy . Canon.toVector $ r
  where
    -- Second @Any@ is really (f (Any))
    co :: SmallArray (f Any) -> SmallArray (I Any)
    co = noInlineUnsafeCo

repToRecord :: Rep I (Record f r) -> Record f r
repToRecord (Rep r) =
    unsafeFromCanonical $ Canon.fromVector . Strict.fromLazy . co $ r
  where
    -- First @Any@ is really (f Any)@
    co :: SmallArray (I Any) -> SmallArray (f Any)
    co = noInlineUnsafeCo

{-------------------------------------------------------------------------------
  Generics instance
-------------------------------------------------------------------------------}

class    (AllFields r (Compose c f), KnownFields r) => RecordConstraints f r c
instance (AllFields r (Compose c f), KnownFields r) => RecordConstraints f r c

recordConstraints :: forall f r c.
     RecordConstraints f r c
  => Proxy c -> Rep (Dict c) (Record f r)
recordConstraints _ = Rep $
    co . aux <$> proxy fieldDicts (Proxy @r)
  where
    aux :: DictAny (Compose c f) -> Dict (Compose c f) Any
    aux DictAny = Dict

    -- The second 'Any' is really (f Any)
    co :: Dict (Compose c f) Any -> Dict c Any
    co = noInlineUnsafeCo

recordMetadata :: forall k (f :: k -> Type) (r :: Row k).
     KnownFields r
  => Metadata (Record f r)
recordMetadata = Metadata {
      recordName          = "Record"
    , recordConstructor   = "Record"
    , recordSize          = length fields
    , recordFieldMetadata = Rep $ smallArrayFromList fields
    }
  where
    fields :: [FieldMetadata Any]
    fields = fieldMetadata (Proxy @r)

instance KnownFields r => Generic (Record f r) where
  type Constraints (Record f r) = RecordConstraints f r
  type MetadataOf  (Record f r) = FieldTypes        f r

  from     = recordToRep
  to       = repToRecord
  dict     = recordConstraints
  metadata = const recordMetadata

{-------------------------------------------------------------------------------
  Instances for standard type classes

  These instances all depend on the generics integration.
-------------------------------------------------------------------------------}

instance RecordConstraints f r Show => Show (Record f r) where
  showsPrec = Generic.gshowsPrec

instance RecordConstraints f r Eq => Eq (Record f r) where
  (==) = Generic.geq

instance ( RecordConstraints f r Eq
         , RecordConstraints f r Ord
         ) => Ord (Record f r) where
  compare = Generic.gcompare

instance RecordConstraints f r ToJSON => ToJSON (Record f r) where
  toJSON = Generic.gtoJSON

instance RecordConstraints f r FromJSON => FromJSON (Record f r) where
  parseJSON = Generic.gparseJSON

{-------------------------------------------------------------------------------
  UTIL. Not sure if we still need this
-------------------------------------------------------------------------------}

{-

data Constrained (c :: k -> Constraint) (f :: k -> Type) (x :: k) where
  Constrained :: c x => f x -> Constrained c f x

constrain :: forall k (c :: k -> Constraint) (f :: k -> Type) (r :: Row k).
      AllFields r c
   => Proxy c -> Record f r -> Record (Constrained c f) r
constrain p (Record.toCanonical -> r) = Record.unsafeFromCanonical $
    Canon.fromLazyVector $
      V.zipWith aux (Canon.toLazyVector r) (fieldDicts (Proxy @r) p)
  where
    aux :: f Any -> DictAny c -> Constrained c f Any
    aux x DictAny = Constrained x

-}

{-------------------------------------------------------------------------------
  Constrained combinators
-------------------------------------------------------------------------------}

cpure :: forall r f c.
     AllFields r c
  => Proxy c
  -> (forall x. c x => f x)
  -> Record f r
cpure p f = map (\Dict -> f) $ reifyAllFields p

cmap :: forall r c f g.
     AllFields r c
  => Proxy c
  -> (forall x. c x => f x -> g x)
  -> Record f r -> Record g r
cmap p f = zipWith (\Dict -> f) (reifyAllFields p)

cmapM ::
     (Applicative m, AllFields r c)
  => Proxy c
  -> (forall x. c x => f x -> m (g x))
  -> Record f r -> m (Record g r)
cmapM p f = sequenceA . cmap p (Comp . f)

toList :: forall r a. KnownFields r => Record (K a) r -> [(String, a)]
toList = Prelude.zipWith aux (fieldMetadata (Proxy @r)) . collapse
  where
    aux :: FieldMetadata b -> a -> (String, a)
    aux (FieldMetadata p _) a = (symbolVal p, a)

czipWithM :: forall m r c f g h.
     (Applicative m, AllFields r c)
  => Proxy c
  -> (forall x. c x => f x -> g x -> m (h x))
  -> Record f r -> Record g r -> m (Record h r)
czipWithM p f r r' =
    sequenceA $ map (fn_2 . f') (reifyAllFields p) `ap` r `ap` r'
  where
    f' :: Dict c x -> f x -> g x -> (m :.: h) x
    f' Dict fx gx = Comp $ f fx gx

czipWith ::
     AllFields r c
  => Proxy c
  -> (forall x. c x => f x -> g x -> h x)
  -> Record f r -> Record g r -> Record h r
czipWith p f a b = unI $ czipWithM p (\x y -> I (f x y)) a b

{-------------------------------------------------------------------------------
  Support for @typelet@
-------------------------------------------------------------------------------}

-- | Introduce type variable for a row
letRecordT :: forall r f.
     (forall r'. Let r' r => Proxy r' -> Record f r)
  -> Record f r
letRecordT f = letT' (Proxy @r) f

-- | Insert field into a record and introduce type variable for the result
letInsertAs :: forall r r' f n a.
     Proxy r       -- ^ Type of the record we are constructing
  -> Field n       -- ^ New field to be inserted
  -> f a           -- ^ Value of the new field
  -> Record f r'   -- ^ Record constructed so far
  -> (forall r''. Let r'' (n := a : r') => Record f r'' -> Record f r)
                   -- ^ Assign type variable to new partial record, and continue
  -> Record f r
letInsertAs _ n x r = letAs' (insert n x r)


