{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Anonymous records
--
-- # OVERVIEW
--
-- Support for anonymous records with the following features:
--
-- 1. Extension (adding new fields)
-- 2. Getter/setter
-- 3. Generics
-- 4. Union
-- 5. HasField
--
-- Removing fields from the record is /not/ a requirement.
--
-- # DESIGN
--
-- We discussed in <https://well-typed.com/blog/2021/10/large-records-part-2/>
-- that when we do type-level induction, it is important that we do so over
-- balanced structures. The examples from that blog post, however, processed
-- /all/ elements from the tree, and it was therefore not important /how/ the
-- tree was balanced, merely /that/ it was balanced. That is not sufficient now:
-- when we want to know the type of a particular field in the record, we don't
-- want to a linear search. We therefore index our records by red-black trees,
-- sorted on the field names (symbols).
--
-- The classic theory of records by Gaster and Jones [1] is a very elegant
-- framework for records in languages with support for rows in the type checker.
-- In Haskell, however, this doesn't work quite so well (unless we write a
-- type checker plugin). Consider G&J-style extension and extraction:
--
-- > insert :: Lacks r a => Proxy k -> a -> Record r -> Record (Insert k a r)
-- > lookup :: Lacks r a => Proxy k -> Record (Insert k a r) -> a
--
-- This has a pleasing symmetry to it, but it means that the Haskell type
-- checker needs to rewrite the type of the argument record to be in the form
-- @Insert k a r@ for some @r@. We therefore opt for a more idiomatic, if
-- perhaps less satisfying, approach:
--
-- > insert :: Proxy k -> a -> Record r -> Insert (k a r)
-- > lookup :: Has k a r => Proxy k -> Record r -> a
--
-- TODO: Discuss whether this has performance consequences.
--
-- # REFERENCES
--
-- [1] "A Polymorphic Type System for Extensible Records and Variants",
--     Gaster and Jones, TR NOTTCS-TR-96-3, 1996.
--     <https://web.cecs.pdx.edu/~mpj/pubs/96-3.pdf>
module Data.Record.Anonymous {- (
    Record -- opaque
  , empty
  , insert
    -- * Type-level indices
  , Empty
  , Insert
  ) -} where

{-
let (r0, xs0) = Empty
    (r1, xs1) = Cons @x0 @xs0 r0
    (r2, xs2) = Cons @x1 @xs1 r1
    ..
    (r, _) = Cons @xN @xsN rN

in r
-}



{-
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import GHC.OverloadedLabels

import Data.Record.Anonymous.RedBlackTree hiding (empty, insert)

-- newtype Record (xs :: Map Symbol Type) = R (Vector Any)

-- variadic?

{-
insert :: l \ r => Rec r -> Rec (l :: a | r)
get    :: l \ r => Rec (l :: a | r) -> a

-- bad idea?
--
-- (because type checker would have to "rewrite" type of arecord to be of the right form?)
get :: FldProxy k -> Record (Insert k a r) -> a

-- instead
get :: Has k a r => Record r ->
-}


newtype Record (xs :: Map Symbol Type) = R (Sing xs)
newtype FldProxy (k :: Symbol) = FldProxy (Sing k)

-- role Record phantom

empty :: Record Empty
empty = R $ singEmpty

-- lots of inserts will /always/ be quadratic?
insert :: FldProxy k -> a -> Record r -> Record (Insert k a r)
insert (FldProxy k) a (R r) = R (singInsert k (SVal a) r)

instance (k ~ k', KnownSymbol k) => IsLabel k (FldProxy k') where
  fromLabel = FldProxy (SSymbol (Proxy @k'))
-}


{-
instance IsLabel l

{-
instance (l ~ l', KnownSymbol l) => IsLabel l (FldProxy l') where
  fromLabel = FldProxy
-}

-}


{-
{-
(
    Rec(..)
  , FldProxy(..)
  , (:=)(..)
    -- * Record construction
  , rnil
  , rcons
  , (&)
    -- * Selection
  , get
  ) where
-}

import Data.Coerce
import Data.Kind
import Data.Vector (Vector)
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.TypeLits
import Unsafe.Coerce

import qualified Data.Vector as V

import Data.Record.Generic



{-
{-------------------------------------------------------------------------------
  BST

  https://www.cs.tufts.edu/comp/150FP/archive/chris-okasaki/redblack99.pdf
-------------------------------------------------------------------------------}

-- | Binary search tree
--
-- We will use this at the type level.
data BST a =
    -- | Empty ree
    Nil

    -- | Singleton tree
  | Leaf a

    -- | Duplicate entry
    --
    -- Invariant for @Dup x xs@: @Root xs ~ 'Just x@
  | Dup a (BST a)

    -- | Branch
    --
    -- Invariant for @Branch l x x@:
    --
    -- o If @Root l ~ 'Just y@, then @y < x@
    -- o If @Root r ~ 'Just y@, then @y > x@
  | Branch (BST a) a (BST a)

type family Root (xs :: BST k) :: Maybe k where
  Root 'Nil            = 'Nothing
  Root ('Leaf x)       = 'Just x
  Root ('Dup x _)      = 'Just x
  Root ('Branch _ x r) = 'Just x

-}

{-------------------------------------------------------------------------------
  G&J style? https://web.cecs.pdx.edu/~mpj/pubs/96-3.pdf

    Insert has a lacks constraint

  Daan style?

    Insert has no constraints

  But in both cases, do we need a Has constraint...?
  If we do

  > get :: Rec (Insert l a r) -> a

  then to apply it, ghc would somehow have to figure out what a suitable choice
  of @r@ is. That is never going to happen.
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
  Insertion
-------------------------------------------------------------------------------}

-- | Insert new element
--

{-
type family Insert (x :: Symbol) (xs :: BST Symbol) :: BST Symbol where
  Insert x  'Nil           = 'Leaf x
  Insert x ('Leaf y)       = InsertLeaf   x   y   (CmpSymbol x y)
  Insert x ('Dup y)        = InsertDup    x   y   (CmpSymbol x y)
  Insert x ('Branch l y r) = InsertBranch x l y r (CmpSymbol x y)

type family InsertLeaf x y ord where
  InsertLeaf x y 'LT = 'Branch ('Leaf x) y 'Nil
  InsertLeaf x y 'EQ = 'Dup x ('Leaf y)
  InsertLeaf x y 'GT = 'Branch 'Nil y ('Leaf x)

type family InsertDup x y ord where
  InsertDup x y 'LT = 'Branch

type family InsertBranch x l y r ord where
  InsertBranch x l y r 'LT = 'Branch (Insert x l) y r
  InsertBranch x l y r 'EQ = 'Dup x ('Branch l y r)
  InsertBranch x l y r 'GT = 'Branch l y (Insert x r)
-}

-- if the outer compare is stuck, it will deeply rewrite the arguments
-- if not, function before arguments (call by name ( but not call by need))

-- insert unknown value into known tree would then result in exponential term?

-- only reveal a redex when you're willing to commit to it
{-
type family Insert (x :: Symbol) (xs :: BST Symbol) :: BST Symbol where
  Insert x 'Nil      = 'Leaf x
  Insert x '(Branch l y x) = Insert' x l y x (CmpSymbol x y)

type type Insert' x l y x (c :: Ordering) where
  Insert' x l y x LessThan = Branch (Insert x l) y r




  Compare x y (Branch .. (Insert x l) ...) .... (Branch .. (Insert x r) ... )

-- | Add field to a record
insert :: l := a -> Rec r -> Rec (Insert l a r)

-}
{-

{-------------------------------------------------------------------------------
  Main type
-------------------------------------------------------------------------------}

-- | Anonymous record
--
-- TODO: Document introduction and elimination forms.
-- TODO: Should we be using something other than Vector? Might want to do a
-- few benchmarks (even though runtime performance isn't critical)
--
-- Thought: @xs@ here has a phantom role, so it ought to be possible to do
-- stuff like @Rec (Without l r)@ without incurring huge overheads.
newtype Rec (xs :: [(Symbol, Type)]) = UnsafeMkRec {
      recToVector :: Vector Any
    }

{-------------------------------------------------------------------------------
  Record construction

  This is loosely based on @superrecord@, although we deviate quite a bit. That
  said, we try to stay close to @superrecord@ where it makes sense, especially
  for user-facing syntax, to ease switching between the two packages.

  Like @jrec@, but unlike @superrecord@, we do not sort field names, so records
  with different field ordering are considered different types.

  TODO: Move away from supperrecord altogether and base this off
  <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf>
  instead?
-------------------------------------------------------------------------------}

-- | Proxy for a label
--
-- Unlike the definition in @superrecord@, we insist on 'KnownSymbol' directly
-- in the definition of the proxy.
data FldProxy (l :: Symbol) = KnownSymbol l => FldProxy

instance (l ~ l', KnownSymbol l) => IsLabel l (FldProxy l') where
  fromLabel = FldProxy

-- | Assignment to a field in a record
--
-- Unlike the definition in @superrecord@, we do _not_ enforce strictness here.
-- Instead, this is done during record construction (so that we could in
-- principle reuse this for lazy records).
--
-- TODO: Show example usage.
data l := t = FldProxy l := t

-- | Empty record
rnil :: Rec '[]
rnil = UnsafeMkRec V.empty

-- | Prepend a field to a record
--
-- Since we are implemented /scoped/ labels, this has no constraints at all.
-- Specifically, we do not require that the label isn't already present: if it
-- is, it will continue to exist in the record, just shadowed by the new one.
--
-- TODO: Think about a non-inductive form that would prevent @n@ array copies
-- for a record construction of @n@ fields (@jrec@ uses tuples for this; we
-- could consider an NP, but of course, not without incurring quadratic core).
-- However, it would nonetheless be good to ensure that this at least scales
-- (compilation-wise).
--
-- TODO: Benchmark this against jrec (and superrecord), for compilation time
-- /and/ runtime. A nested record will still incur O(n^2) here, due to the
-- type annotations, but I would nonetheless expect it to be significantly
-- better since the type is much, much simpler. That said, we should have a way
-- to avoid quadratic code entirely (ideally also solving the problem above).
-- I guess the tuples could help here.
rcons :: l := a -> Rec r -> Rec ('(l, a) ': r)
rcons (_l := t) (UnsafeMkRec v) = UnsafeMkRec $ V.cons (unsafeCoerce t) v

-- | Alias for 'rcons'
(&) :: l := a -> Rec r -> Rec ('(l, a) ': r)
(&) = rcons

infixr 5 &

{-------------------------------------------------------------------------------
  Selection
-------------------------------------------------------------------------------}




--type family IndexOf (r :: [(Symbol, Type)]) (l :: Symbol) :: Nat where
--
--type Has (r :: [(Symbol, Type)]) (l :: Symbol) a = () :: Constraint
--  KnownNat


{-
Something like

> KnownNat (IndexOf l r)

would result in a large coercion just for the evidence
(and we don't even know the result type yet)
-}




get :: Has r l a => FldProxy l -> Rec r -> a
get = undefined


{-------------------------------------------------------------------------------
  Generic instance
-------------------------------------------------------------------------------}

class    ConstraintsRec (r :: [(Symbol, Type)]) (c :: Type -> Constraint) where
instance ConstraintsRec r c where

instance Generic (Rec r) where
  type Constraints (Rec r) = ConstraintsRec r
  type MetadataOf  (Rec r) = r

  dict     = undefined
  metadata = undefined

  from = coerce
  to   = coerce

-}
-}
