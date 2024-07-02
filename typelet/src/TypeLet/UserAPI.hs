{-# LANGUAGE TypeApplications #-}

module TypeLet.UserAPI (
    -- * Main classes
    Let
  , Equal
  , castEqual
    -- * Introduction forms
  , LetT(..)
  , letT
  , letT'
  , constructLet
  , LetAs(..)
  , letAs
  , letAs'
    -- * Re-exports
  , Proxy(..)
  ) where

import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

-- $setup
-- >>> :set -fplugin=TypeLet
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables

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
-- * The left-hand side is not a type variable
--
-- >>> let aux :: Let Int Int => () ; aux = castEqual () in aux
-- ...
-- ...Let with non-variable LHS...
-- ...
--
-- >>> :{
--   \(x :: a) ->
--     let
--       y :: Let a Int => a
--       y = castEqual (1 :: Int)
--     in y
-- :}
-- ...
-- ...Let with non-variable LHS...
-- ...
--
-- * The set of let-bindings in scope are cyclic.
--
-- >>> :{
--   let cycle :: (Let a b, Let b a) => (a, b) -> (a, b)
--       cycle (a, b) = (castEqual b, castEqual a)
--   in cycle ('a', 'b')
-- :}
-- ...
-- ...Cycle in type-level let bindings: a := b, b := a
-- ...
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
-- See discussion of 'Equal' for additional information.
--
-- Note: without additional 'Let' constraints in scope, 'Equal' constraints
-- simply resolve to unification constraints:
--
-- >>> (castEqual :: Int -> Bool) 1
-- ...
-- ...Couldn't match...Int...Bool...
-- ...
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
-- careful thought; see also 'constructLet'.
letT' :: forall r a. Proxy a -> (forall b. Let b a => Proxy b -> r) -> r
{-# NOINLINE letT' #-}
letT' = letAs'

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
letAs' fa k = k fa

-- | Dual to 'letAs''
--
-- Where 'letAs'' /takes/ an existing value and then /introduces/ a type
-- variable, 'constructLet' is used to /produce/ a value and then /eliminate/ a
-- type variable.
--
-- Consider constructing a heterogenous list @[x, y, z]@. Without the @typelet@
-- library this might look something like
--
-- > hlist :: HList '[X, Y, Z]
-- > hlist =
-- >     HCons @X @'[Y, Z] x
-- >   $ HCons @Y @'[   Z] y
-- >   $ HCons @Z @'[    ] z
-- >   $ HNil
--
-- The problem here is that tail list argument to @HCons@, and causes this
-- example to be quadratic in size. With @letAs'@ we could write this as
--
-- > hlist :: HList '[X, Y, Z]
-- > hlist =
-- >   letAs' (HCons @Z @'[] z Nil) $ \(xs2 :: HList ts2) ->
-- >   letAs' (HCons @Y @ts2 y xs2) $ \(xs1 :: HList ts1) ->
-- >   letAs' (HCons @X @ts1 x xs1) $ (\xs0 :: HList ts0) ->
-- >   castEqual xs0
--
-- Here we are using @letAs'@ to introduce a type variable for each partial
-- list, thereby avoiding the repeated lists in the type arguments. However,
-- if we write it like this, there is an additional repeated list in the
-- implicit continuation type argument @r@ to @letAs'@; making that argument
-- explicit, the above code is really this:
--
-- > hlist :: HList '[X, Y, Z]
-- > hlist =
-- >   letAs' @(HList '[X, Y, Z]) (HCons @Z @'[] z Nil) $ \(xs2 :: HList ts2) ->
-- >   letAs' @(HList '[X, Y, Z]) (HCons @Y @ts2 y xs2) $ \(xs1 :: HList ts1) ->
-- >   letAs' @(HList '[X, Y, Z]) (HCons @X @ts1 x xs1) $ (\xs0 :: HList ts0) ->
-- >   castEqual xs0
--
-- The solution is to introduce one more type variable for the whole list, and
-- use that as the top-level:
--
-- > hlist :: HList '[X, Y, Z]
-- > hlist = constructLet $ \(_ :: Proxy ts) ->
-- >   letAs' @(HList ts) (HCons @Z @'[] z Nil) $ \(xs2 :: HList ts2) ->
-- >   letAs' @(HList ts) (HCons @Y @ts2 y xs2) $ \(xs1 :: HList ts1) ->
-- >   letAs' @(HList ts) (HCons @X @ts1 x xs1) $ (\xs0 :: HList ts0) ->
-- >   castEqual xs0
--
-- Note that none of these type arguments are necessary; we merely showed them
-- to illustrate what is going on. The final example can be written as shown
-- below, and will be linear in size:
--
-- > hlist :: HList '[X, Y, Z]
-- > hlist = constructLet $
-- >   letAs' (HCons z Nil) $ \xs2 ->
-- >   letAs' (HCons y xs2) $ \xs1 ->
-- >   letAs' (HCons x xs1) $ (xs0 ->
-- >   castEqual xs0
constructLet :: forall f a. (forall b. Let b a => Proxy b -> f b) -> f a
constructLet f = f Proxy



