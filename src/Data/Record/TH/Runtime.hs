{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

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
  , Construct(FieldsTuple, construct)
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

-- | Typeclass instead of construct_X because of explicit imports/exports
--
-- Note: We use type family instead of class parameter because
-- the head of instance shouldn't contain type familes
--
-- Example:
-- 
-- > type family F a b = r | r -> a where
-- >   F a b = Maybe b
-- >
-- > largeRecord .. [d|
-- >   data X a = X { a, b :: F a Int }
-- >  |]
-- >
-- > x = [lr| X { a = Just 1, b = Just 2 } |]
--
-- generates
--
-- > newtype X a = LR__X ...
-- >
-- > instance Construct (X a) where
-- >   type FieldsTuple (X a) = (F a Int, F b Int)
-- >   construct ~_ (a, b) = ...
-- >
-- > x = construct (LR__X undefined) (Just 1, Just 2)
class Construct record where
  type FieldsTuple record 
  construct :: record -> FieldsTuple record -> record

