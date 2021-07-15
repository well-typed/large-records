{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions to support the TH code (i.e., functions called by generated code)
--
-- NOTE: We leave the generic representation type as lazy, and only force
-- values once we translate back to the type itself. This means that we can
-- chain generic functions and get some kind of fusion without having to
-- traverse records multiple times.
module Data.Record.TH.Runtime (
    -- * Miscellaneous
    dictFor
  , repFromVector
  , repToVector
  , rnfVectorAny
  , noInlineUnsafeCo
  , ThroughLRGenerics(..)
  ) where

import Data.Coerce (coerce)
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V

import Data.Record.Generic

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

dictFor :: c x => Proxy c -> Proxy x -> Dict c x
dictFor _ _ = Dict

repFromVector :: Vector Any -> Rep I a
repFromVector = coerce

repToVector :: Rep I a -> Vector Any
repToVector = coerce

rnfVectorAny :: Vector Any -> ()
rnfVectorAny = rnfElems . V.toList
  where
    rnfElems :: [Any] -> ()
    rnfElems []     = ()
    rnfElems (x:xs) = x `seq` rnfElems xs

-- | Avoid potential segfault with ghc < 9.0
--
-- See <https://gitlab.haskell.org/ghc/ghc/-/issues/16893>.
-- I haven't actually seen this fail in large-records, but we saw it fail in
-- the compact representation branch of sop-core, and what we do here is not
-- so different, so better to play it safe.
noInlineUnsafeCo :: forall a b. a -> b
{-# NOINLINE noInlineUnsafeCo #-}
noInlineUnsafeCo = unsafeCoerce

-- | Route from GHC generics to LR generics
--
-- Suppose a function such as
--
-- > allEqualTo :: Eq a => a -> [a] -> Bool
-- > allEqualTo x = all (== x)
--
-- is instead written as
--
-- > allEqualTo :: (GHC.Generic a, GHC.GEq' (GHC.Rep a)) => a -> [a] -> Bool
-- > allEqualTo x = all (GHC.geqdefault x)
--
-- where instead of using an indirection through an auxiliary type class `Eq`,
-- it directly assumes @GHC.Generics@ and uses a concrete generic
-- implementation. Such design is arguably questionable, but for example
-- @beam-core@ contains many such deeply ingrained assumptions of the
-- availability of @GHC.Generics@.
--
-- In order to be able to call such a function on a large record @Foo@,
-- 'largeRecord' will generate an instance
--
-- > instance GHC.Generic Foo where
-- >   type Rep Foo = ThroughLRGenerics Foo
-- >
-- >   from = WrapThroughLRGenerics
-- >   to   = unwrapThroughLRGenerics
--
-- For our running example, this instance makes it possible to call 'allEqualTo'
-- provided we then provide an instance
--
-- > instance ( LR.Generic a
-- >          , LR.Constraints a Eq
-- >          ) => GHC.GEq' (ThroughLRGenerics a) where
-- >   geq' = LR.geq `on` unwrapThroughLRGenerics
--
-- Effectively, 'ThroughLRGenerics' can be used to redirect a function that uses
-- GHC generics to a function that uses LR generics.
newtype ThroughLRGenerics a p = WrapThroughLRGenerics {
      unwrapThroughLRGenerics :: a
    }