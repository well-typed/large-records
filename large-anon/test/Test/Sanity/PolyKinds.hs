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
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Sanity.PolyKinds (tests) where

import Data.Bifunctor.Flip
import Data.Kind
import Data.Maybe (fromJust)
import Data.SOP
import Data.SOP.Dict

import Data.Record.Anonymous.Advanced (Record, Pair((:=)), KnownFields, AllFields)
import Data.Record.Anonymous.Existential

import qualified Data.Record.Anonymous.Advanced as Anon

import Test.Tasty
import Test.Tasty.HUnit

import Test.Sanity.Existential.DynRecord
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
    , testCase "merge"      test_merge
    ]

-- | Test generics ('AllFields' and 'KnownFields')
test_show :: Assertion
test_show =
    assertEqual "" expected $
      show exampleRecord1'
  where
    expected :: String
    expected = "Record {a = True, b = 1234}"

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
    expected = "Record {a = 1, b = True}"

-- | Merging
test_merge :: Assertion
test_merge =
    assertEqual "" exampleRecord1' $
      Anon.project $ Anon.merge exampleRecord2 exampleRecord3

{-------------------------------------------------------------------------------
  Auxiliary: marking strictness at the type level
-------------------------------------------------------------------------------}

data MarkStrictness a = Strict a | Lazy a

data Boxed :: MarkStrictness Type -> Type where
  BoxStrict :: !a -> Boxed (Strict a)
  BoxLazy   ::  a -> Boxed (Lazy   a)

instance Show a => Show (Boxed (Strict a)) where
  show (BoxStrict x) = show x
instance Show a => Show (Boxed (Lazy a)) where
  show (BoxLazy x) = show x

instance Eq a => Eq (Boxed (Strict a)) where
  BoxStrict x == BoxStrict y = x == y
instance Eq a => Eq (Boxed (Lazy a)) where
  BoxLazy x == BoxLazy y = x == y

instance Parse Boxed (Strict Int) where
  parseField = fmap (BoxStrict . unI) . Dyn.parseField

instance Parse Boxed (Lazy Bool) where
  parseField = fmap (BoxLazy . unI) . Dyn.parseField

{-------------------------------------------------------------------------------
  Example: @large-anon@ provides only fully strict records out of the box. We
  can turn this into fully strict records by wrapping every field in a lazy box.
  With a little bit of work we can also support mixed records, recording
  strictness at the type-level.
-------------------------------------------------------------------------------}

exampleRecord1, exampleRecord1' ::
  Record Boxed [ "a" := Lazy   Bool
               , "b" := Strict Int
               ]
exampleRecord1 =
      Anon.insert #a (BoxLazy undefined)
    $ Anon.insert #b (BoxStrict 1234)
    $ Anon.empty
exampleRecord1' =
      Anon.insert #a (BoxLazy True)
    $ Anon.insert #b (BoxStrict 1234)
    $ Anon.empty

exampleRecord2 :: Record Boxed '[ "b" := Strict Int ]
exampleRecord2 =
      Anon.insert #b (BoxStrict 1234)
    $ Anon.empty

exampleRecord3 :: Record Boxed '[ "a" := Lazy Bool ]
exampleRecord3 =
      Anon.insert #a (BoxLazy True)
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
      (SomeField @(Strict Int)  mkDicts)
      (SomeField @(Lazy   Bool) mkDicts)
  where
    mkDicts ::
         (Show (Boxed a), Parse Boxed a)
      => NP (Flip Dict a) '[Compose Show Boxed, Parse Boxed]
    mkDicts = Flip Dict :* Flip Dict :* Nil

parseSomeRecord :: DynRecord -> SomeRecord
parseSomeRecord dyn =
    case recoverType dyn of
      SomeFields p -> SomeRecord p $ fromJust (Dyn.parse dyn)

