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

-- | Test type-level metadata
--
-- It is important that we have a separate test for this, because we don't
-- actually rely on this information anywhere; it is only for SOP interop.
module Test.Sanity.TypeLevelMetadata (tests) where

import Data.Record.Generic
import Data.Record.Generic.SOP
import Data.Kind
import Data.Typeable
import GHC.TypeLits

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous.Advanced (Pair((:=)))

import qualified Data.Record.Anonymous.Simple   as S
import qualified Data.Record.Anonymous.Advanced as A

tests :: TestTree
tests = testGroup "Test.Sanity.TypeLevelMetadata" [
      testCase "metadata"  test_metadata
    , testCase "toFromSOP" test_toFromSOP
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

recordA :: A.Record I [ "a" := Bool, "b" := Char ]
recordA =
      A.insert #a (I True)
    $ A.insert #b (I 'a')
    $ A.empty

recordS :: S.Record [ "a" := Bool, "b" := Char ]
recordS =
      S.insert #a True
    $ S.insert #b 'a'
    $ S.empty

{-------------------------------------------------------------------------------
  Auxiliary infrastructure

  Obviously the 'GetMetadata' instances are inductive and will result in
  quadratic core code blowup; they are here only for testing.
-------------------------------------------------------------------------------}

type family Fst (p :: (k1, k2)) :: k1 where Fst '(x, y) = x
type family Snd (p :: (k1, k2)) :: k2 where Snd '(x, y) = y

class GetMetadata (xs :: [(Symbol, Type)]) where
  getMetadata :: Proxy xs -> [(String, String)]

instance GetMetadata '[] where
  getMetadata _ = []

instance ( KnownSymbol (Fst x)
         , Typeable (Snd x)
         , GetMetadata xs
         ) => GetMetadata (x ': xs) where
  getMetadata _ =
        (symbolVal (Proxy @(Fst x)), show (typeRep (Proxy @(Snd x))))
      : getMetadata (Proxy @xs)

-- | Reflect field metadata from the type-level information
getMetadataA :: forall f r.
     GetMetadata (A.FieldTypes f r)
  => A.Record f r  -- ^ Serves as a proxy only
  -> [(String, String)]
getMetadataA _ = getMetadata (Proxy @(A.FieldTypes f r))

-- | Like 'getMetadataA', but for the simple API
getMetadataS :: forall r.
     GetMetadata (S.SimpleFieldTypes r)
  => S.Record r  -- ^ Serves as a proxy only
  -> [(String, String)]
getMetadataS _ = getMetadata (Proxy @(S.SimpleFieldTypes r))

{-------------------------------------------------------------------------------
  Tests proper

  TODO: We should have a test somewhere for normalization; this too depends
  on having accurate type-level information (it's what it was introduced for
  in the first place).
-------------------------------------------------------------------------------}

test_metadata :: Assertion
test_metadata = do
    assertEqual "advanced" expectedA $
      getMetadataA recordA
    assertEqual "simple" expectedS $
      getMetadataS recordS
  where
    expectedA, expectedS :: [(String, String)]
    expectedA = [
          ("a", "I Bool")
        , ("b", "I Char")
        ]
    expectedS = [
          ("a", "Bool")
        , ("b", "Char")
        ]

test_toFromSOP :: Assertion
test_toFromSOP =
    assertEqual "" (Just recordA) $
      (to . fromSOP <$> toSOP (from recordA))
