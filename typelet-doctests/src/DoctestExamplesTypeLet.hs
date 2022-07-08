{-# OPTIONS_GHC -fplugin=TypeLet #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Doctests to verify the plugin's custom type errors
--
module DoctestExamplesTypeLet where

import TypeLet

-- $setup
-- >>> :set -fplugin=TypeLet
-- >>> :set -XScopedTypeVariables
-- >>> import TypeLet

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

-- | Examples of casts between simple different types
--
-- >>> (castEqual :: Int -> Bool) 1
-- ...
-- ...Couldn't match...Int...Bool...
-- ...
--
-- >>> (castEqual :: Int -> Char) 1
-- ...
-- ...Couldn't match...Int...Char...
-- ...
simpleMismatch :: ()
simpleMismatch = ()

-- | Let with non-variable left-hand side
--
-- NOTE: We see this error multiple times, but that seems to be a doctest thing
-- rather than anything the plugin does: the more doctest test cases I add, the
-- more often I see the error repeated. In regular (non-doctest) code I don't
-- see those repeats.
--
-- >>> let aux :: Let Int Int => () ; aux = castEqual () in aux
-- ...
-- ...Let with non-variable LHS: Int := Int
-- ...
letWithNonVarLHS :: ()
letWithNonVarLHS = ()

-- | Recursive let bindings
--
-- I'm not actually sure how these could arise in practice, especially when we
-- have the skolem check in place as well. But perhaps it's possible with some
-- cunning scheme where we introduce regular (unification) variables that then
-- unify to skolem variables? For now we err on the side of caution and detect
-- this explicitly; we /may/ want to reconsider that at some point if the check
-- turns out to be expensive in practice.
--
-- >>> :{
--   let cycle :: (Let a b, Let b a) => (a, b) -> (a, b)
--       cycle (a, b) = (castEqual b, castEqual a)
--   in cycle ('a', 'b')
-- :}
-- ...
-- ...Cycle in type-level let bindings: a := b, b := a
-- ...
letCycle :: ()
letCycle = ()

-- | Non-skolem LHS
--
-- See 'NonSkolemLHS' for details.
--
-- >>> :{
--   \(x :: a) ->
--     let
--       y :: Let a Integer => a
--       y = castEqual (1 :: Integer)
--     in y
-- :}
-- ...
-- ...Let with non-skolem LHS: a := Integer
-- ...
letWithNonSkolemLHS :: ()
letWithNonSkolemLHS = ()

{-------------------------------------------------------------------------------
  Sanity check
-------------------------------------------------------------------------------}

-- | Make sure the plugin works (independent of doctest)
sanityCheck :: Int
sanityCheck = castEqual (1 :: Int)

main :: IO ()
main = print sanityCheck
