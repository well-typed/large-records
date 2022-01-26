{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Record.Anonymous.Sanity.TypeLevelMetadata (tests) where

import Data.Record.Generic
import Data.Record.Generic.SOP
import Data.Kind
import Data.Typeable
import GHC.TypeLits

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity.TypeLevelMetadata" [
      testCase "metadata"  test_metadata
    , testCase "toFromSOP" test_toFromSOP
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

recordA :: Record I '[ '("a", Bool), '("b", Char) ]
recordA =
      insert #a (I True)
    $ insert #b (I 'a')
    $ empty

{-------------------------------------------------------------------------------
  Auxiliary infrastructure

  Obviously the 'ReflectMetadata' instances are inductive and will result in
  quadratic core code blowup; they are here only for testing.
-------------------------------------------------------------------------------}

type family Fst (p :: (k1, k2)) :: k1 where Fst '(x, y) = x
type family Snd (p :: (k1, k2)) :: k2 where Snd '(x, y) = y

class ReflectMetadata (xs :: [(Symbol, Type)]) where
  reflectMetadata :: Proxy xs -> [(String, String)]

instance ReflectMetadata '[] where
  reflectMetadata _ = []

instance ( KnownSymbol (Fst x)
         , Typeable (Snd x)
         , ReflectMetadata xs
         ) => ReflectMetadata (x ': xs) where
  reflectMetadata _ =
        (symbolVal (Proxy @(Fst x)), show (typeRep (Proxy @(Snd x))))
      : reflectMetadata (Proxy @xs)

-- | Reflect field metadata from the type-level information
reflectFieldMetadata :: forall f r.
     ReflectMetadata (RecordMetadataOf f r)
  => Record f r  -- ^ Serves as a proxy only
  -> [(String, String)]
reflectFieldMetadata _ = reflectMetadata (Proxy @(RecordMetadataOf f r))

{-------------------------------------------------------------------------------
  Tests proper

  TODO: We should have a test somewhere for normalization; this too depends
  on having accurate type-level information (it's what it was introduced for
  in the first place).
-------------------------------------------------------------------------------}

test_metadata :: Assertion
test_metadata =
    assertEqual "" expected $ reflectFieldMetadata recordA
  where
    expected :: [(String, String)]
    expected = [
          ("a", "I Bool")
        , ("b", "I Char")
        ]

test_toFromSOP :: Assertion
test_toFromSOP =
    assertEqual "" (Just recordA) $
      (to . fromSOP <$> toSOP (from recordA))

