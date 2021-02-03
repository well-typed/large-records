{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.RecordConstruction (tests) where

import Data.Record.TH

import Test.Tasty

largeRecord defaultLazyOptions [d|
    data R = MkR { x :: Int, y :: Bool }
  |]

-- TODO: Check that we can't introduce type errors (unsafe coercing True to Int or whatever)

-- This call just indicates to @ghc@ that we have reached the end of a binding
-- group, and so it should process all definitions. This is not necessary if
--
-- * There is another call to 'largeRecord' (or any other TH splice) in between
--   the record definition and its use
-- * The record definition and the record use are in different modules.
--
-- TODO: It'd be nicer if we could avoid this altogether.
endOfBindingGroup


inOrder :: R
inOrder = $(constructRecord [| MkR { x = 1234, y = True } |])

outOfOrder :: R
outOfOrder = $(constructRecord [| MkR { y = True, x = 1234 } |])

-- Damn. TH doesn't type check, but it checks that names are in scope :/
z :: Int
z = 5

-- Results in "Unexpected fields" error
-- extraFields :: R
-- extraFields = $(constructRecord [| MkR { x = 1234, y = True, z = () } |])

-- But this works (with a warning)
missingFields :: R
missingFields = $(constructRecord [| MkR { x = 1234 } |])

tests :: TestTree
tests = testGroup "Test.Record.Sanity.RecordConstruction" []
