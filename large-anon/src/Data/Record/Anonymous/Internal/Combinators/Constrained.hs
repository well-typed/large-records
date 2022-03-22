{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Constrained combinators
--
-- These combinators provide evidence for @c x@ for each field of type @f x@.
-- This is a non-trivial design choice which which requires some justification:
--
-- 1. More primitive
--
-- Consider 'cpure' versus @cpureF@ (hypothetical, not offered by the lib):
--
-- > cpure  :: .. -> (forall x. c    x  => f x) -> Record f r
-- > cpureF :: .. -> (forall x. c (f x) => f x) -> Record f r
--
-- At first glance, @cpureF@ might seem like the more logical choice; after all,
-- each field in the record has type @f x@ for some @x@. However, @cpureF@ is
-- easily defined in terms of 'cpure', but the reverse is is much more awkward.
--
-- 2. More natural
--
-- Consider 'cmap':
--
-- > cmap :: .. (forall x. c x => f x -> g x) -> Record f r -> Record g r
--
-- What should the type of the callback be? Any of the following could be useful
-- in different circumstances:
--
-- > cmapF :: .. -> (forall x. (c (f x)         ) => f x -> g x) -> ..
-- > cmapF :: .. -> (forall x.           c (g x)) => f x -> g x) -> ..
-- > cmapF :: .. -> (forall x. (c (f x), c (g x)) => f x -> g x) -> ..
--
-- None of these is more general then the other (the third version is the most
-- general from the point of view of the callback, but the most restrictive
-- from the point of view of the caller of 'cmap').
--
-- 3. More in line with the combinators on 'Rep'
--
-- Compare
--
-- > cpure :: .. -> (forall x. c x => f x) -> Rep    f a
-- > cpure :: .. -> (forall x. c x => f x) -> Record f r
--
-- The @large-generics@ infrastructure is defined over types of kind @Type@,
-- not over types of kind @(Type -> Type) -> Type@, and so the 'Constraints'
-- /cannot/ include a functor argument. That doesn't mean of course that
-- 'cpure' could not be defined as
--
-- > cpure :: (Generic a, Constraints a (Compose c f)) => ..
--
-- but in the context of @large-generics@ the functor-less version is very
-- natural (and works well). That doesn't necessarily mean 'cpure' for 'Record'
-- should follow suit, of course, but it's nice that they do line up.
module Data.Record.Anonymous.Internal.Combinators.Constrained (
    -- * Applicative
    cpure
    -- * Functor
  , cmap
  , cmapM
    -- * Zipping
  , czipWith
  , czipWithM
  ) where

import Data.Proxy
import Data.SOP.BasicFunctors

import Data.Record.Anonymous.Internal.Constraints
import Data.Record.Anonymous.Internal.Record (Record)
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Internal.Combinators.Simple as Simple

{-------------------------------------------------------------------------------
  Applicative
-------------------------------------------------------------------------------}

cpure :: forall r f c.
     (AllFields r c, KnownFields r)
  => Proxy c
  -> (forall x. c x => f x)
  -> Record f r
cpure p f = Simple.map aux (constrain p (Simple.pure (K ())))
  where
    aux :: Constrained c (K ()) x -> f x
    aux (Constrained _) = f

{-------------------------------------------------------------------------------
  Functor
-------------------------------------------------------------------------------}

cmap :: forall r c f g.
     AllFields r c
  => Proxy c
  -> (forall x. c x => f x -> g x)
  -> Record f r -> Record g r
cmap p f = Simple.map aux . constrain p
  where
    aux :: Constrained c f x -> g x
    aux (Constrained x) = f x

cmapM ::
     (Monad m, AllFields r c)
  => Proxy c
  -> (forall x. c x => f x -> m (g x))
  -> Record f r -> m (Record g r)
cmapM p f = Simple.sequenceA . cmap p (Comp . f)

{-------------------------------------------------------------------------------
  Zipping
-------------------------------------------------------------------------------}

czipWithM :: forall m r c f g h.
     (Monad m, AllFields r c)
  => Proxy c
  -> (forall x. c x => f x -> g x -> m (h x))
  -> Record f r -> Record g r -> m (Record h r)
czipWithM p f r r' = Simple.zipWithM aux (constrain p r) r'
  where
    aux :: Constrained c f x -> g x -> m (h x)
    aux (Constrained x) y = f x y

czipWith ::
     AllFields r c
  => Proxy c
  -> (forall x. c x => f x -> g x -> h x)
  -> Record f r -> Record g r -> Record h r
czipWith p f a b = unI $ czipWithM p (\x y -> I (f x y)) a b
