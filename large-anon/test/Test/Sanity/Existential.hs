{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Sanity.Existential (tests) where

import Data.Record.Generic
import Data.Maybe (fromJust)
import Data.SOP
import Data.Bifunctor.Flip

import qualified Data.Record.Generic.Rep as Rep

import Data.Record.Anonymous.Simple (Record, KnownFields, AllFields)
import Data.Record.Anonymous.Existential

import Test.Tasty
import Test.Tasty.HUnit

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Existential" [
      testCase "showParsed" test_showParsed
    ]

test_showParsed :: Assertion
test_showParsed = do
    print $ parseSomeRecord $ DynRecord [("a", VI 1), ("b", VB True)]

{-------------------------------------------------------------------------------
  Example dynamically typed record

  TODO: include projection discovery, and then model Sam's example: parse
  some stuff into a record, project out known stuff, leaving the rest
  (forwards compatibility), and then update the full thing again.
-------------------------------------------------------------------------------}

data DynRecord = DynRecord [(String, Value)]

data Value = VI Int | VB Bool

recoverType :: DynRecord -> SomeFields '[Show, Parse]
recoverType (DynRecord xs) = someFields (map aux xs)
  where
    aux :: (String, Value) -> SomeField '[Show, Parse]
    aux (n, VI _) = SomeField n (mkDicts @Int)
    aux (n, VB _) = SomeField n (mkDicts @Bool)

    mkDicts :: forall a. (Show a, Parse a) => NP (Flip Dict a) '[Show, Parse]
    mkDicts = Flip Dict :* Flip Dict :* Nil

{-------------------------------------------------------------------------------
  Parsing (using to previously discovered type)
-------------------------------------------------------------------------------}

class Parse a where
  parseField :: Value -> Maybe a

instance Parse Int where
  parseField (VI x) = Just x
  parseField _      = Nothing

instance Parse Bool where
  parseField (VB x) = Just x
  parseField _      = Nothing

parseRecord :: forall r.
     (KnownFields r, AllFields r Parse)
  => DynRecord -> Maybe (Record r)
parseRecord (DynRecord xs) =
    to <$> Rep.sequenceA rep
  where
    md = metadata (Proxy @(Record r))

    rep :: Rep (Maybe :.: I) (Record r)
    rep = Rep.cmap (Proxy @Parse) getField (recordFieldNames md)
      where
        getField :: Parse x => K String x -> (Maybe :.: I) x
        getField (K name) = Comp $ do
            v <- lookup name xs
            I <$> parseField v

{-------------------------------------------------------------------------------
  Discover type and parse
-------------------------------------------------------------------------------}

data SomeRecord where
  SomeRecord ::
       (KnownFields r, AllFields r Show, AllFields r Parse)
    => Proxy r -> Record r -> SomeRecord

instance Show SomeRecord where
  show (SomeRecord _ r) = show r

parseSomeRecord :: DynRecord -> SomeRecord
parseSomeRecord dyn =
    case recoverType dyn of
      SomeFields p -> SomeRecord p (fromJust (parseRecord dyn))
