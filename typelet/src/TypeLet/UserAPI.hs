module TypeLet.UserAPI (
    -- * Main classes
    Let
  , Equal
  , castEqual
    -- * Introduction forms
  , LetT(..)
  , letT
  , letT'
  , LetAs(..)
  , letAs
  , letAs'
    -- * Re-exports
  , Proxy(..)
  ) where

import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
  Main classes
-------------------------------------------------------------------------------}

-- | Type-level @let@
--
-- A constraint @Let x t@ where @x@ is an (existential) type variable and @t@
-- is an arbitrary type represents a type-level let binding @let x = t@.
--
-- o Introduction form
--
--   Type-level let bindings should be introduced using 'letT' or its slightly
--   higher level cousin, 'letAs'.
--
-- o Elimination form
--
--   To eliminate type-level let, use 'castEqual'.
class Let (a :: k) (b :: k)

-- | Reflexivity
--
-- A constraint @Let x t@, where @x@ is an existential (skolem) type variable
-- and @t@ is an arbitrary type, models a type-level @let x = t@. There is only
-- a single instance for @Let@: reflexivity (a type is equal to itself).
--
-- User code normally does not work with @Let@ directly, but instead uses one of
-- the introduction forms ('letT' and 'letAs') which take care to introduce
-- @Let@ constraints of the right shape. When @Let@ constraints are introduced
-- manually, the plugin will report a type error if
--
-- * The left-hand side is not a skolem type variable
-- * The set of let-bindings in scope are cyclic.
instance Let a a

-- | (Nominal) type equality, up to type-level let
--
-- This is a class without any definitions; 'Equal a b' is instead proved by
-- the plugin. Suppose we have a bunch of type-level let constraints in scope
--
-- > Let x1 t1
-- > Let x2 t2
-- > ...
--
-- Then if σ is the corresponding idempotent substitution, two types @a@ and @b@
-- are considered 'Equal' if @σ(a)@ and @σ(b)@ are nominally equal.
class Equal (a :: k) (b :: k)

-- | Type-safe cast, using 'Equal' as the notion of equality
--
-- See comments for 'Equal'.
castEqual :: Equal a b => a -> b
-- Implementation note: marking this as NOINLINE in an attempt to make sure that
-- @unsafeCoerce@ does not escape the scope of the evidence for @Equal@ (which,
-- even though trivial, at least records the full time of the LHS and RHS).
{-# NOINLINE castEqual #-}
castEqual = unsafeCoerce

{-------------------------------------------------------------------------------
  Introduction forms
-------------------------------------------------------------------------------}

-- | 'LetT' is used along with 'letT' to introduce type-level let bindings.
--
-- See 'letT' for more information.
data LetT (a :: k) where
  LetT :: Let b a => Proxy b -> LetT a

-- | Primitive way to introduce type-level let binding.
--
-- Usage:
--
-- > case letT (Proxy @t) of LetT (_ :: Proxy x) ->
--
-- This introduces a type-level let binding @x = t@.
letT :: Proxy a -> LetT a
{-# NOINLINE letT #-}
letT p = LetT p

-- | CPS form of 'letT'
--
-- While this is more convenient to use, the @r@ parameter itself requires
-- careful thought.
letT' :: forall r a. Proxy a -> (forall b. Let b a => Proxy b -> r) -> r
{-# NOINLINE letT' #-}
letT' pa k = case letT pa of LetT pb -> k pb

-- | Used together with 'letAs' to pair a type-level let binding with a cast
--
-- See 'letAs' for details.
data LetAs f (a :: k) where
  LetAs :: Let b a => f b -> LetAs f a

-- | Pair a type-level let binding with a cast
--
-- Often when we introduce a type-level let @x = t@, we then want to cast some
-- term @e :: t@ to a term @e :: x@; function 'letAs' does these two things in
-- one operation.
--
-- If we did the above as written, however, we would have a term @e :: x@ where
-- we know nothing about @x@ at all (unless we cast again). This is typically
-- not useful; instead, we go from a term @e :: f t@ to a term @e :: f x@,
-- let-binding the type index @t@ but not the functor @f@.
letAs :: f a -> LetAs f a
{-# NOINLINE letAs #-}
letAs x = LetAs x

-- | CPS form of 'letAs'
--
-- See also comments for 'letT''.
letAs' :: forall r f a. f a -> (forall b. Let b a => f b -> r) -> r
{-# NOINLINE letAs' #-}
letAs' fa k = case letAs fa of LetAs fb -> k fb
