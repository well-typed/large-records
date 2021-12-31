{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

-- | Balanced trees
--
-- This is an implementation of red-black trees as described by Chris Okasaki
--
--   "Red-Black Trees in a Functional Setting", Chris Okasaki, JFP 1(1) 1993
--
-- We enforce the red/black through types, similar to
--
-- - Depending on Types, Stephanie Weirich
--   <https://www.codemesh.io/codemesh2015/stephanie-weirich>
-- - A Red-Black Tree Implementation with Provable Properties, Frank Staals
--   <https://fstaals.net/RedBlackTree.html>
--
-- although our implementation is different in the details from both of these.
--
-- Intended for qualified import.
module Data.Record.Anonymous.RedBlackTree (
    Map -- opaque
    -- * Term-level
  , empty
  , insert
  , lookup
  , fromList
  , toList
    -- * Type-level
  , Empty
  , Insert
  , Lookup
  , FromList
    -- ** Relation to term level
  , Sing(..)
  , singEmpty
  , singInsert
  , singLookup
  ) where

import Prelude hiding (lookup)

import Data.Function (on)
import Data.Kind
import Data.List (foldl')
import GHC.TypeLits hiding (Nat)

import Data.Record.Anonymous.Util.Singleton
import Data.Record.Anonymous.Util.Promotion

{-------------------------------------------------------------------------------
  Red-black tree

  Core types
-------------------------------------------------------------------------------}

data Color = B | R
data Nat   = Z | S Nat

-- | Red-black tree
--
-- A red-black tree is an encoding of a 2-3-4 tree, with 3-nodes and 4-nodes
-- represented as small clusters of 2-nodes: one black parent, and one or two
-- red children (see "A dichromatic framework for balanced trees", Guibas and
-- Sedgewick, SFCS '78).
--
-- This encoding is enforced by two separate invariants: red nodes must have
-- black children, and the root of the tree must be black. The 'RedBlack' type
-- only enforces the former; the 'Map' top-level wrapper enforces the latter.
data RedBlack :: Color -> Nat -> Type -> Type where
  E  :: RedBlack B Z a
  NE :: NonEmpty c d a -> RedBlack c d a

-- | Non-empty red-black tree
--
-- The red/black invariants are guaranteed through the types. Together, they
-- imply that the tree must be roughly balanced: the longest path from the
-- root to any leaf is at most twice as long as the shortest.
data NonEmpty :: Color -> Nat -> Type -> Type where
  -- | Black node
  --
  -- Black nodes increase the black depth, and may have black or red children.
  NB :: RedBlack c1 d a -> a -> RedBlack c2 d a -> NonEmpty B (S d) a

  -- | Red node
  --
  -- Red nodes leave the black depth alone, and can only have black children.
  NR :: RedBlack B d a -> a -> RedBlack B d a -> NonEmpty R d a

{-------------------------------------------------------------------------------
  Insertion
-------------------------------------------------------------------------------}

-- | Red-black tree with a possible red-violation near the root
data PossibleViolation :: Color -> Nat -> Type -> Type where
  -- | Violation on the left
  VL :: NonEmpty R d a -> a -> RedBlack B d a -> PossibleViolation 'R d a

  -- | Violation on the right
  VR :: RedBlack B d a -> a -> NonEmpty R d a -> PossibleViolation 'R d a

  -- | No violation
  VN :: RedBlack B d a -> a -> RedBlack B d a -> PossibleViolation 'R d a

possibleViolationLeft :: NonEmpty c d a -> a -> RedBlack B d a -> PossibleViolation 'R d a
possibleViolationLeft (NB a x b) y c = VN (NE (NB a x b)) y c
possibleViolationLeft (NR a x b) y c = VL (NR a x b) y c

possibleViolationRight :: RedBlack B d a -> a -> NonEmpty c d a -> PossibleViolation 'R d a
possibleViolationRight a x (NB b y c) = VN a x (NE (NB b y c))
possibleViolationRight a x (NR b y c) = VR a x (NR b y c)

-- | Result of inserting a new node into a tree
data Inserted :: Color -> Nat -> Type -> Type where
  -- | Value inserted into a black tree
  --
  -- When we insert into a black tree, the result might be red, /but/ it will
  -- not have any invariant violations
  IB :: NonEmpty c d a -> Inserted B d a

  -- | Value inserted into a red tree
  --
  -- When we insert into a red tree, we might have an invariant violation near
  -- the top. We therefore record the left and right subtrees separately,
  -- without insisting on their color. We will fix the violation once we get
  -- to their (black) parent.
  IR :: PossibleViolation R n a -> Inserted R n a

-- | The four cases in which the tree might become unbalanced
--
-- We inserted something into a black or a red node, either on the left or on
-- the right; hence, four cases.
data Unbalanced :: Color -> Nat -> Type -> Type where
  UB_Lft :: Inserted c1 d a -> a -> RedBlack c2 d a -> Unbalanced B (S d) a
  UB_Rgt :: RedBlack c1 d a -> a -> Inserted c2 d a -> Unbalanced B (S d) a
  UR_Lft :: Inserted B  d a -> a -> RedBlack B  d a -> Unbalanced R    d  a
  UR_Rgt :: RedBlack B  d a -> a -> Inserted B  d a -> Unbalanced R    d  a

-- | Balancing
--
-- The exact rebalancing rules are shown in
--
--   "Red-Black Trees in a Functional Setting", Chris Okasaki, JFP 1(1) 1993
--
-- in Figure 1, "Eliminating red nodes with red parents".
balance :: Unbalanced c d a -> Inserted c d a

-- Rotation cases (rotation only happens when inserting into black node)
balance (UB_Lft (IR (VR a x (NR b y c))) z d) = IB $ NR (NE (NB a x b)) y (NE (NB c z d))
balance (UB_Lft (IR (VL (NR a x b) y c)) z d) = IB $ NR (NE (NB a x b)) y (NE (NB c z d))
balance (UB_Rgt a x (IR (VR b y (NR c z d)))) = IB $ NR (NE (NB a x b)) y (NE (NB c z d))
balance (UB_Rgt a x (IR (VL (NR b y c) z d))) = IB $ NR (NE (NB a x b)) y (NE (NB c z d))

-- Fall-through cases for insertion into black nodes
balance (UB_Lft (IB a) x b)          = IB $ NB (NE a) x b
balance (UB_Rgt a x (IB b))          = IB $ NB a x (NE b)
balance (UB_Lft (IR (VN a x b)) y c) = IB $ NB (NE (NR a x b)) y c
balance (UB_Rgt a x (IR (VN b y c))) = IB $ NB a x (NE (NR b y c))

-- Fall-through cases for insertion into red nodes
balance (UR_Lft (IB a) x b) = IR $ possibleViolationLeft  a x b
balance (UR_Rgt a x (IB b)) = IR $ possibleViolationRight a x b

-- | Low-level insertion function
--
-- New nodes are always inserted at the leaves of the tree, and the new nodes
-- are initially always coloured red. Moreover, as we reconstruct the spine of
-- the tree (along the path to the new node), the nodes either /preserve/ their
-- color, or turn red (during rebalancing).
--
-- Since we do not create any new black nodes during this process, the black
-- invariant cannot be violated. We do however create new /red/ nodes, and must
-- therefore check that have a black parent, and if they don't, rebalance the
-- tree so that they do.
--
-- When we have a red parent with a red child, we need to insert a black node,
-- but if we do this without an existing black node, we would be increasing the
-- black depth of the tree instead of preserving it. We therefore wait until
-- we see the black /grandparent/ of the red child, and only then rebalance.
insertRedBlack :: forall c n a.
     (a -> a -> Ordering)
  -> a -> RedBlack c n a -> Inserted c n a
insertRedBlack f x = go
  where
    go :: forall c' n'. RedBlack c' n' a -> Inserted c' n' a
    go  E              = IB $ NR E x E
    go (NE (NB l y r)) = case f x y of
                           LT -> balance $ UB_Lft (go l) y r
                           GT -> balance $ UB_Rgt l y (go r)
                           EQ -> IB $ NB l x r
    go (NE (NR l y r)) = case f x y of
                           LT -> balance $ UR_Lft (go l) y r
                           GT -> balance $ UR_Rgt l y (go r)
                           EQ -> IR $ VN l x r

{-------------------------------------------------------------------------------
  Tree queries

  These are strictly term-level; for type-level implementation, see below.
-------------------------------------------------------------------------------}

data Tree :: Type -> Type where
  Tree :: RedBlack c d a -> Tree a

match :: Tree a -> Maybe (Tree a, a, Tree a)
match (Tree t) = go t
  where
    go :: RedBlack c n a -> Maybe (Tree a, a, Tree a)
    go E               = Nothing
    go (NE (NB l x r)) = Just (Tree l, x, Tree r)
    go (NE (NR l x r)) = Just (Tree l, x, Tree r)

pattern TE :: Tree a
pattern TE <- (match -> Nothing)

pattern TN :: Tree a -> a -> Tree a -> Tree a
pattern TN l x r <- (match -> Just (l, x, r))

{-# COMPLETE TE, TN #-}

find :: forall a. (a -> Ordering) -> Tree a -> Maybe a
find f = go
  where
    go :: Tree a -> Maybe a
    go TE         = Nothing
    go (TN l x r) = case f x of
                      LT -> go l
                      GT -> go r
                      EQ -> Just x

inorder :: Tree a -> [a]
inorder = go . (:[])
  where
    -- We maintain a work-list of trees to visit
    go :: [Tree a] -> [a]
    go []              = []
    go (TE : ts)       = go ts
    go (TN l x r : ts) = x : go (l : r : ts)

{-------------------------------------------------------------------------------
  User-facing wrapper
-------------------------------------------------------------------------------}

-- | Map
data Map k a where
  MkMap :: RedBlack B n (k, a) -> Map k a

-- | Empty map
empty :: Map k a
empty = MkMap E

-- | Insert
--
-- If the key already exists, the old value is overwritten.
insert :: Ord k => k -> a -> Map k a -> Map k a
insert = \k x (MkMap t) -> aux (insertRedBlack (compare `on` fst) (k, x) t)
  where
    -- By insisting that the root of the tree is always black, we ensure that
    -- inserting a new node cannot result in an invariant violation near the
    -- tip. This means that all we have to do here is simply to color the root
    -- black (if it's not black already); this is only place during insertion
    -- where the black depth increases.
    aux :: Inserted B d (k, a) -> Map k a
    aux (IB (NB l x r)) = MkMap $ NE (NB l x r)
    aux (IB (NR l x r)) = MkMap $ NE (NB l x r)

-- | Key lookup
lookup :: Ord k => k -> Map k a -> Maybe a
lookup k (MkMap t) = snd <$> find (compare k . fst) (Tree t)

fromList :: Ord k => [(k, a)] -> Map k a
fromList = foldl' (flip (uncurry insert)) empty

toList :: Map k a -> [(k, a)]
toList (MkMap t) = inorder (Tree t)

{-------------------------------------------------------------------------------
  Type-level implementation

  The implementation of balance is a near copy-and-paste of the term-level
  definition.

  In the definition of insertion and lookup we need to be a bit more careful,
  however. Suppose we tried something like the below instead:

  > Insert k x (NE (NB l '(k', y) r)) = Compare k k' (.. Insert k x l ..) (..
  >   Insert k x r ..) ..

  When the @Compare@ is not stuck, @ghc@ will evaluate the function call before
  the arguments (call-by-need style), and all is well. But if it /is/ stuck,
  then @ghc@ will evaluate the arguments as far as it can (in the hope that it
  can make some progress). Now consider what happens when we tried to insert an
  unknown (type variable) field into a concrete large tree. The @Compare@ term
  would be stuck, and @ghc@ would try to insert in both the left /and/ the right
  subtree; since this process would be repeated at every node in the tree (the
  field is unknown, after all, and so every comparison would be stuck) this
  would result in a term that is O(2^n) in size (yes, exponential, not
  quadratic). The lesson here is not to expose any redex to @ghc@ unless we know
  that we want to evaluate it.

  In addition, insertion and lookup are less polymorphic than their term-level
  cousins, due to the inability to partially apply type-level functions. We
  could use defunctionalization, but we don't need the generalization for our
  purposes, so we leave it monomorphic (that is, monokinded) in the key.
-------------------------------------------------------------------------------}

type family PossibleViolationLeft (l :: NonEmpty c d a) (x :: a) (r :: RedBlack B d a) :: PossibleViolation R d a where
  PossibleViolationLeft (NB a x b) y c = VN (NE (NB a x b)) y c
  PossibleViolationLeft (NR a x b) y c = VL (NR a x b) y c

type family PossibleViolationRight (l :: RedBlack B d a) (x :: a) (r :: NonEmpty c d a) :: PossibleViolation R d a where
  PossibleViolationRight a x (NB b y c) = VN a x (NE (NB b y c))
  PossibleViolationRight a x (NR b y c) = VR a x (NR b y c)

type family Balance (t :: Unbalanced c d a) :: Inserted c d a where
  -- Rotation cases (rotation only happens when inserting into black node)
  Balance (UB_Lft (IR (VR a x (NR b y c))) z d) = IB (NR (NE (NB a x b)) y (NE (NB c z d)))
  Balance (UB_Lft (IR (VL (NR a x b) y c)) z d) = IB (NR (NE (NB a x b)) y (NE (NB c z d)))
  Balance (UB_Rgt a x (IR (VR b y (NR c z d)))) = IB (NR (NE (NB a x b)) y (NE (NB c z d)))
  Balance (UB_Rgt a x (IR (VL (NR b y c) z d))) = IB (NR (NE (NB a x b)) y (NE (NB c z d)))

  -- Fall-through cases for insertion into black nodes
  Balance (UB_Lft (IB a) x b)          = IB (NB (NE a) x b)
  Balance (UB_Rgt a x (IB b))          = IB (NB a x (NE b))
  Balance (UB_Lft (IR (VN a x b)) y c) = IB (NB (NE (NR a x b)) y c)
  Balance (UB_Rgt a x (IR (VN b y c))) = IB (NB a x (NE (NR b y c)))

  -- Fall-through cases for insertion into red nodes
  Balance (UR_Lft (IB a) x b) = IR (PossibleViolationLeft  a x b)
  Balance (UR_Rgt a x (IB b)) = IR (PossibleViolationRight a x b)

type family InsertRedBlack (k :: Symbol) (x :: a) (t :: RedBlack c n (Symbol, a)) :: Inserted c n (Symbol, a) where
  InsertRedBlack k x  E                     = IB (NR E '(k, x) E)
  InsertRedBlack k x (NE (NB l '(k', y) r)) = InsertBlack k x l k' y r (CmpSymbol k k')
  InsertRedBlack k x (NE (NR l '(k', y) r)) = InsertRed   k x l k' y r (CmpSymbol k k')

type family InsertBlack (k   :: Symbol)
                        (x   :: a)
                        (l   :: RedBlack c1 d (Symbol, a))
                        (k'  :: Symbol)
                        (y   :: a)
                        (r   :: RedBlack c2 d (Symbol, a))
                        (cmp :: Ordering)
                     :: Inserted c (S d) (Symbol, a) where
  InsertBlack k x l k' y r LT = Balance (UB_Lft (InsertRedBlack k x l) '(k', y) r)
  InsertBlack k x l k' y r GT = Balance (UB_Rgt l '(k', y) (InsertRedBlack k x r))
  InsertBlack k x l k' y r EQ = IB (NB l '(k, x) r)

type family InsertRed (k   :: Symbol)
                      (x   :: a)
                      (l   :: RedBlack B d (Symbol, a))
                      (k'  :: Symbol)
                      (y   :: a)
                      (r   :: RedBlack B d (Symbol, a))
                      (cmp :: Ordering)
                   :: Inserted c d (Symbol, a) where
  InsertRed k x l k' y r LT = Balance (UR_Lft (InsertRedBlack k x l) '(k', y) r)
  InsertRed k x l k' y r GT = Balance (UR_Rgt l '(k', y) (InsertRedBlack k x r))
  InsertRed k x l k' y r EQ = IR (VN l '(k, x) r)

type family FromInserted (t :: Inserted B d (k, a))
                      :: Map k a where
  FromInserted (IB (NB l x r)) = MkMap (NE (NB l x r))
  FromInserted (IB (NR l x r)) = MkMap (NE (NB l x r))

type family Empty :: Map Symbol a where
  Empty = MkMap E

type family Insert (k :: Symbol)
                   (x :: a)
                   (t :: Map Symbol a)
                :: Map Symbol a where
  Insert k x (MkMap t) = FromInserted (InsertRedBlack k x t)

type family Find (k :: Symbol) (t :: RedBlack c d (Symbol, a)) :: Maybe a where
  Find k  E                     = Nothing
  Find k (NE (NB l '(k', x) r)) = FindNonEmpty k l k' x r (CmpSymbol k k')
  Find k (NE (NR l '(k', x) r)) = FindNonEmpty k l k' x r (CmpSymbol k k')

type family FindNonEmpty (k   :: Symbol)
                         (l   :: RedBlack c1 d (Symbol, a))
                         (k'  :: Symbol)
                         (x   :: a)
                         (r   :: RedBlack c2 d (Symbol, a))
                         (cmp :: Ordering)
                      :: Maybe a where
  FindNonEmpty k l k' x r LT = Find k l
  FindNonEmpty k l k' x r GT = Find k r
  FindNonEmpty k l k' x r EQ = Just x

type family Lookup (k :: Symbol)
                   (t :: Map Symbol a)
                :: Maybe a where
  Lookup k (MkMap t) = Find k t

-- | Convert list to map
--
-- This is intentionally a left fold, to make a type-level FromList easier to
-- interpret:
--
-- > FromList '[
-- >     '("t00", T 00)
-- >   , '("t01", T 01)
-- >   , '("t02", T 02)
-- >   ]
--
-- Means the insertiom of t00, then t01, then t02.
type family FromList (xs :: [(Symbol, a)]) :: Map Symbol a where
  FromList xs = FromListAcc Empty xs

type family FromListAcc (acc :: Map Symbol a)
                        (xs  :: [(Symbol, a)])
                     :: Map Symbol a where
  FromListAcc acc '[]            = acc
  FromListAcc acc ('(k, x) : xs) = FromListAcc (Insert k x acc) xs

{-------------------------------------------------------------------------------
  Singleton instances
-------------------------------------------------------------------------------}

data instance Sing :: RedBlack c n a -> Type where
  SE  :: Sing E
  SNE :: Sing (t :: NonEmpty c n a)
      -> Sing (NE t :: RedBlack c n a)

data instance Sing :: NonEmpty c n a -> Type where
  SNB :: Sing (l :: RedBlack c1 d a)
      -> Sing (x :: a)
      -> Sing (r :: RedBlack c2 d a)
      -> Sing (NB l x r :: NonEmpty B (S d) a)
  SNR :: Sing (l :: RedBlack B d a)
      -> Sing (x :: a)
      -> Sing (r :: RedBlack B d a)
      -> Sing (NR l x r :: NonEmpty R d a)

data instance Sing :: PossibleViolation c n a -> Type where
  SVL :: Sing (l :: NonEmpty R d a)
      -> Sing (x :: a)
      -> Sing (r :: RedBlack B d a)
      -> Sing (VL l x r :: PossibleViolation 'R d a)
  SVR :: Sing (l :: RedBlack B d a)
      -> Sing (x :: a)
      -> Sing (r :: NonEmpty R d a)
      -> Sing (VR l x r :: PossibleViolation 'R d a)
  SVN :: Sing (l :: RedBlack B d a)
      -> Sing (x :: a)
      -> Sing (r :: RedBlack B d a)
      -> Sing (VN l x r :: PossibleViolation 'R d a)

data instance Sing :: Inserted c n a -> Type where
  SIB :: Sing (t :: NonEmpty c d a)
      -> Sing (IB t :: Inserted B d a)
  SIR :: Sing (t :: PossibleViolation R n a)
      -> Sing (IR t :: Inserted R n a)

data instance Sing :: Unbalanced c n a -> Type where
  SUB_Lft :: Sing (l :: Inserted c1 d a)
          -> Sing (x :: a)
          -> Sing (r :: RedBlack c2 d a)
          -> Sing (UB_Lft l x r :: Unbalanced B (S d) a)
  SUB_Rgt :: Sing (l :: RedBlack c1 d a)
          -> Sing (x :: a)
          -> Sing (r :: Inserted c2 d a)
          -> Sing (UB_Rgt l x r :: Unbalanced B (S d) a)
  SUR_Lft :: Sing (l :: Inserted B d a)
          -> Sing (x :: a)
          -> Sing (r :: RedBlack B d a)
          -> Sing (UR_Lft l x r :: Unbalanced R d a)
  SUR_Rgt :: Sing (l :: RedBlack B d a)
          -> Sing (x :: a)
          -> Sing (r :: Inserted B d a)
          -> Sing (UR_Rgt l x r :: Unbalanced R d a)

data instance Sing :: Map k a -> Type where
  SMkMap :: Sing (t :: RedBlack B n (k, a)) -> Sing (MkMap t :: Map k a)

{-------------------------------------------------------------------------------
  Promotion
-------------------------------------------------------------------------------}

type instance Promoted (RedBlack c n a) = RedBlack c n (Promoted a)
type instance Promoted (NonEmpty c n a) = NonEmpty c n (Promoted a)
type instance Promoted (Map k a)        = Map (Promoted k) (Promoted a)

instance Bounce a => Bounce (RedBlack c n a) where
  promote E      = SomeSing $ SE
  promote (NE t) = case promote t of
                     SomeSing t' -> SomeSing $ SNE t'

  demote  SE     = E
  demote (SNE t) = NE (demote t)

instance Bounce a => Bounce (NonEmpty c n a) where
  promote (NB l x r) = case (promote l, promote x, promote r) of
                         (SomeSing l', SomeSing x', SomeSing r') ->
                           SomeSing $ SNB l' x' r'
  promote (NR l x r) = case (promote l, promote x, promote r) of
                         (SomeSing l', SomeSing x', SomeSing r') ->
                           SomeSing $ SNR l' x' r'

  demote (SNB l x r) = NB (demote l) (demote x) (demote r)
  demote (SNR l x r) = NR (demote l) (demote x) (demote r)

instance (Bounce k, Bounce a) => Bounce (Map k a) where
  promote (MkMap t) = case promote t of
                        SomeSing t' -> SomeSing $ SMkMap t'
  demote (SMkMap x) = MkMap (demote x)

{-------------------------------------------------------------------------------
  Show that our type level functions are well-defined
-------------------------------------------------------------------------------}

singEmpty :: Sing (Empty :: Map Symbol a)
singEmpty = SMkMap SE

singPossibleViolationLeft ::
     Sing (l :: NonEmpty c d a)
  -> Sing (x :: a)
  -> Sing (r :: RedBlack B d a)
  -> Sing (PossibleViolationLeft l x r :: PossibleViolation R d a)
singPossibleViolationLeft (SNB a x b) y c = SVN (SNE (SNB a x b)) y c
singPossibleViolationLeft (SNR a x b) y c = SVL (SNR a x b) y c

singPossibleViolationRight ::
     Sing (l :: RedBlack B d a)
  -> Sing (x :: a)
  -> Sing (r :: NonEmpty c d a)
  -> Sing (PossibleViolationRight l x r :: PossibleViolation R d a)
singPossibleViolationRight a x (SNB b y c) = SVN a x (SNE (SNB b y c))
singPossibleViolationRight a x (SNR b y c) = SVR a x (SNR b y c)

singBalance ::
     Sing (t :: Unbalanced c d a)
  -> Sing (Balance t :: Inserted c d a)
singBalance (SUB_Lft (SIR (SVR a x (SNR b y c))) z d) = SIB $ SNR (SNE (SNB a x b)) y (SNE (SNB c z d))
singBalance (SUB_Lft (SIR (SVL (SNR a x b) y c)) z d) = SIB $ SNR (SNE (SNB a x b)) y (SNE (SNB c z d))
singBalance (SUB_Rgt a x (SIR (SVR b y (SNR c z d)))) = SIB $ SNR (SNE (SNB a x b)) y (SNE (SNB c z d))
singBalance (SUB_Rgt a x (SIR (SVL (SNR b y c) z d))) = SIB $ SNR (SNE (SNB a x b)) y (SNE (SNB c z d))
singBalance (SUB_Lft (SIB a) x b)           = SIB $ SNB (SNE a) x b
singBalance (SUB_Rgt a x (SIB b))           = SIB $ SNB a x (SNE b)
singBalance (SUB_Lft (SIR (SVN a x b)) y c) = SIB $ SNB (SNE (SNR a x b)) y c
singBalance (SUB_Rgt a x (SIR (SVN b y c))) = SIB $ SNB a x (SNE (SNR b y c))
singBalance (SUR_Lft (SIB a) x b) = SIR $ singPossibleViolationLeft  a x b
singBalance (SUR_Rgt a x (SIB b)) = SIR $ singPossibleViolationRight a x b

singInsertRedBlack ::
     Sing (k :: Symbol)
  -> Sing (x :: a)
  -> Sing (t :: RedBlack c n (Symbol, a))
  -> Sing (InsertRedBlack k x t :: Inserted c n (Symbol, a))
singInsertRedBlack k x SE = SIB (SNR SE (SPair k x) SE)
singInsertRedBlack k x (SNE (SNB l (SPair k' y) r)) =
    case axiomCmpSymbol k k' of
      SLT -> singBalance (SUB_Lft (singInsertRedBlack k x l) (SPair k' y) r)
      SGT -> singBalance (SUB_Rgt l (SPair k' y) (singInsertRedBlack k x r))
      SEQ -> SIB (SNB l (SPair k x) r)
singInsertRedBlack k x (SNE (SNR l (SPair k' y) r)) =
    case axiomCmpSymbol k k' of
      SLT -> singBalance (SUR_Lft (singInsertRedBlack k x l) (SPair k' y) r)
      SGT -> singBalance (SUR_Rgt l (SPair k' y) (singInsertRedBlack k x r))
      SEQ -> SIR (SVN l (SPair k x) r)

singInsert :: forall a k x t.
     Sing (k :: Symbol)
  -> Sing (x :: a)
  -> Sing (t :: Map Symbol a)
  -> Sing (Insert k x t :: Map Symbol a)
singInsert = \k x (SMkMap t) -> aux (singInsertRedBlack k x t)
  where
    aux :: forall d t'.
           Sing (t' :: Inserted B d (Symbol, a))
        -> Sing (FromInserted t' :: Map Symbol a)
    aux (SIB (SNB l x r)) = SMkMap (SNE (SNB l x r))
    aux (SIB (SNR l x r)) = SMkMap (SNE (SNB l x r))

singFind ::
     Sing (k :: Symbol)
  -> Sing (t :: RedBlack c d (Symbol, a))
  -> Sing (Find k t :: Maybe a)
singFind _ SE = SNothing
singFind k (SNE (SNB l (SPair k' x) r)) =
    case axiomCmpSymbol k k' of
      SLT -> singFind k l
      SGT -> singFind k r
      SEQ -> SJust x
singFind k (SNE (SNR l (SPair k' x) r)) =
    case axiomCmpSymbol k k' of
      SLT -> singFind k l
      SGT -> singFind k r
      SEQ -> SJust x

singLookup ::
     Sing (k :: Symbol)
  -> Sing (t :: Map Symbol a)
  -> Sing (Lookup k t :: Maybe a)
singLookup k (SMkMap t) = singFind k t
