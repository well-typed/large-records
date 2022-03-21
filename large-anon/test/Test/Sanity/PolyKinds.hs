{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Sanity.PolyKinds (tests) where

import Data.Kind
import Data.Maybe (fromJust)

import Data.Record.Anonymous.Advanced (Record, KnownFields, AllFields)
import Data.Record.Anonymous.Existential

import qualified Data.Record.Anonymous.Advanced as Anon

import Test.Tasty
import Test.Tasty.HUnit

import Test.Sanity.Existential.DynRecord
import Data.Bifunctor.Flip
import Data.SOP.Dict
import Data.SOP

import qualified Test.Sanity.Existential.DynRecord as Dyn

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.PolyKinds" [
      testCase "hasField"   test_hasField
    , testCase "show"       test_show
    , testCase "project"    test_project
    , testCase "showParsed" test_showParsed
    ]

-- | Test generics ('AllFields' and 'KnownFields')
test_show :: Assertion
test_show =
    assertEqual "" expected $
      show exampleRecord1'
  where
    expected :: String
    expected = "Record {a = BoxLazy True, b = BoxStrict 1234}"

-- | 'HasField' (and 'KnownHash', but that's no different for polykinds)
test_hasField :: Assertion
test_hasField = do
    assertEqual "get" (BoxStrict 1234) $
      Anon.get #b $ exampleRecord1
    assertEqual "set" exampleRecord1' $
      Anon.set #a (BoxLazy True) $ exampleRecord1

-- | 'Project'
--
-- NOTE: The projection must ignore the undefined value.
test_project :: Assertion
test_project =
    assertEqual "" exampleRecord2 $
      Anon.project exampleRecord1

-- | Type discovery
test_showParsed :: Assertion
test_showParsed =
    assertEqual "" expected $
      show $ parseSomeRecord $ DynRecord [
          ("a", Dyn.VI 1)
        , ("b", Dyn.VB True)
        ]
  where
    expected = "Record {a = BoxStrict 1, b = BoxLazy True}"

{-------------------------------------------------------------------------------
  Auxiliary: marking strictness at the type level
-------------------------------------------------------------------------------}

data Strict = Strict | Lazy

data Boxed :: (Strict, Type) -> Type where
  BoxStrict :: !a -> Boxed '( 'Strict , a )
  BoxLazy   ::  a -> Boxed '( 'Lazy   , a )

deriving instance Show a => Show (Boxed '(strict, a))
deriving instance Eq   a => Eq   (Boxed '(strict, a))

instance Parse Boxed '( 'Strict, Int ) where
  parseField = fmap (BoxStrict . unI) . Dyn.parseField

instance Parse Boxed '( 'Lazy, Bool ) where
  parseField = fmap (BoxLazy . unI) . Dyn.parseField

{-------------------------------------------------------------------------------
  Example: @large-anon@ provides only fully strict records out of the box. We
  can turn this into fully strict records by wrapping every field in a lazy box.
  With a little bit of work we can also support mixed records, recording
  strictness at the type-level.
-------------------------------------------------------------------------------}

exampleRecord1, exampleRecord1' ::
  Record Boxed '[ '("a", '( 'Lazy   , Bool ))
                , '("b", '( 'Strict , Int  ))
                ]
exampleRecord1 =
      Anon.insert #a (BoxLazy undefined)
    $ Anon.insert #b (BoxStrict 1234)
    $ Anon.empty
exampleRecord1' =
      Anon.insert #a (BoxLazy True)
    $ Anon.insert #b (BoxStrict 1234)
    $ Anon.empty

exampleRecord2 :: Record Boxed '[ '("b", '( 'Strict , Int)) ]
exampleRecord2 =
      Anon.insert #b (BoxStrict 1234)
    $ Anon.empty

{-------------------------------------------------------------------------------
  Type recovery

  Just for an example, we infer all 'Int' fields are strict and all 'Bool'
  fields as lazy.
-------------------------------------------------------------------------------}

data SomeRecord where
  SomeRecord ::
       ( KnownFields r
       , AllFields r (Compose Show Boxed)
       , AllFields r (Parse Boxed)
       )
    => Proxy r -> Record Boxed r -> SomeRecord

instance Show SomeRecord where
  show (SomeRecord _ r) = show r

recoverType :: DynRecord -> SomeFields '[Compose Show Boxed, Parse Boxed]
recoverType =
    Dyn.inferType
      (SomeField @'( 'Strict , Int)  mkDicts)
      (SomeField @'( 'Lazy   , Bool) mkDicts)
  where
    mkDicts :: forall (strict :: Strict) (a :: Type).
         (Show a, Parse Boxed '(strict, a))
      => NP (Flip Dict '(strict, a)) '[Compose Show Boxed, Parse Boxed]
    mkDicts = Flip Dict :* Flip Dict :* Nil

parseSomeRecord :: DynRecord -> SomeRecord
parseSomeRecord dyn =
    case recoverType dyn of
      SomeFields p -> SomeRecord p $ fromJust (Dyn.parse dyn)

