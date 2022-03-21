{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Sanity.Existential (tests) where

import Data.Bifunctor.Flip
import Data.Kind
import Data.Maybe (fromJust)
import Data.Record.Generic
import Data.SOP

import Data.Record.Anonymous.Simple (Record, KnownFields, AllFields)
import Data.Record.Anonymous.Existential

import Test.Tasty
import Test.Tasty.HUnit

import Test.Sanity.Existential.DynRecord (DynRecord(..), Parse)
import qualified Test.Sanity.Existential.DynRecord as Dyn

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Existential" [
      testCase "showParsed" test_showParsed
    ]

test_showParsed :: Assertion
test_showParsed =
    assertEqual "" expected $
      show $ parseSomeRecord $ DynRecord [
          ("a", Dyn.VI 1)
        , ("b", Dyn.VB True)
        ]
  where
    expected = "Record {a = 1, b = True}"

{-------------------------------------------------------------------------------
  Type recovery

  TODO: include projection discovery, and then model Sam's example: parse
  some stuff into a record, project out known stuff, leaving the rest
  (forwards compatibility), and then update the full thing again.
-------------------------------------------------------------------------------}

data SomeRecord where
  SomeRecord ::
       ( KnownFields r
       , AllFields r Show
       , AllFields r (Parse I)
       )
    => Proxy r -> Record r -> SomeRecord

instance Show SomeRecord where
  show (SomeRecord _ r) = show r

recoverType :: DynRecord -> SomeFields '[Show, Parse I]
recoverType =
    Dyn.inferType
      (SomeField @Int  mkDicts)
      (SomeField @Bool mkDicts)
  where
    mkDicts :: forall (a :: Type).
         (Show a, Parse I a)
      => NP (Flip Dict a) '[Show, Parse I]
    mkDicts = Flip Dict :* Flip Dict :* Nil

parseSomeRecord :: DynRecord -> SomeRecord
parseSomeRecord dyn =
    case recoverType dyn of
      SomeFields p -> SomeRecord p $ fromJust (Dyn.parseSimple dyn)
