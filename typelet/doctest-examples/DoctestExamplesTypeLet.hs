{-# OPTIONS_GHC -fplugin=TypeLet #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Doctests to verify the plugin's custom type errors
--
-- Testing ghc type errors is a bit of a quagmire. For some use cases,
-- deferred type errors can be used to turn the compile time errors into
-- runtime errors which can then be tested in the usual way. However, this does
-- not work terribly well for constraints. There are various tickets open about
-- this, which (frustratingly) pull in opposite directions:
--
-- * <https://gitlab.haskell.org/ghc/ghc/-/issues/11197>
-- * <https://gitlab.haskell.org/ghc/ghc/-/issues/16249>
--
-- So instead we use docstring to do so, but that too is not that easy to get
-- working. The first problem is the right parameters must be passed to doctest,
-- in particular, the package flags. There are multiple ways to achieve this:
--
-- * <https://github.com/sol/doctest#running-doctest-for-a-cabal-package>
--
--   The readme on github suggests to run
--
--   > cabal repl --with-ghc=doctest
--
--   but this doesn't work terribly well with cabal's dependency resolution.
--
-- * <https://hackage.haskell.org/package/doctest>
--
--   The readme of the released package suggests simply constructing arguments
--   by hand
--
--   > main = doctest ["-isrc", "src/Main.hs"]
--
--   but this is painful; a realistic set of arguments, especially package
--   dependencies, is hard to construct manually in such a way that it
--   correctly integrates with dependency resolution.
--
-- * <https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md>
--
--   @cabal-docspec@ is an entirely alternative solution, but is not released.
--   It might warrant a closer look though; in particular, the fact that it
--   avoids interpreted code might help with the "self-dependency" issue listed
--   below (see also <https://github.com/sol/doctest/pull/217>).
--
-- * <https://hackage.haskell.org/package/cabal-doctest>
--
--   Technically deprecated, although it seems that it's being maintained still
--   and Oleg informs me that Andres Abel "undeprecated" it, whatever that means.
--
-- The last option seems to be the best, at present, but here two there are some
-- hurdles, some minor, some more significant.
--
-- * The setup instructions are pretty good, but the documentation is a little
--   confusing. In particular, the argument to @defaultMainWithDoctests@
--
--   <https://hackage.haskell.org/package/cabal-doctest-1.0.9/docs/Distribution-Extra-Doctest.html#v:defaultMainWithDoctests>
--
--   /must/ be the name of the /cabal component/ that is the test-suite
--   that is meant to run doctest; if that is wrong, simply nothing happens
--   (no @Build_doctests@ module is built, but nor is there an error message).
--
-- * cabal-doctest supports running doctest on the main library in a cabal file
--   as well as on executables, /but not on test suites/ (though it would be a
--   minor patch to address this). This matters because of the next problem:
--
-- * It seems to be impossible to load the @TypeLet@ plugin defined /in/ the
--   library while doctesting that same library. Using
--
--   > >>> :set -fplugin=TypeLet
--   > >>> (castEqual :: Int -> Int) 1
--
--   is accepted in the sense that ghc doesn't complain about TypeLet being an
--   unknown plugin (unlike when that name is changed to a non-existent
--   plugin), but the plugin nonetheless doesn't actually seem to load:
--   this examples leads to error about a missing @Equal Int Int@ instance.
--   I still do not understand what's happening here.
--
-- * We therefore have the doctests in this regular (non test-suite) module.
--   Of course, users would not need that executable, so both this example
--   executable as well as the doctest test suite that runs doctest /on/
--   this example executable as only built if with @+build-doctest-examples@.
module Main where

import TypeLet

-- $setup
-- >>> :set -fplugin=TypeLet
-- >>> :set -XScopedTypeVariables

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