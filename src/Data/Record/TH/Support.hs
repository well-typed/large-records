{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

-- | Functions to support the TH code (i.e., functions called by generated code)
--
-- NOTE: We leave the generic representation type as lazy, and only force
-- values once we translate back to the type itself. This means that we can
-- chain generic functions and get some kind of fusion without having to
-- traverse records multiple times.
module Data.Record.TH.Support (
    -- * Miscellaneous
    dictFor
  , repFromVector
  , repToVector
  , rnfVectorAny
    -- * Infrastructure for supporting matching on records
  , MatchHasField -- opaque
  , matchHasField
  , fieldNamed
  ) where

import Data.Coerce (coerce)
import Data.Kind
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Any)
import GHC.Records.Compat
import GHC.TypeLits

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

{-------------------------------------------------------------------------------
  Infrastructure for supporting matching on records

  We should be careful not to reintroduce quadratic code size here.
-------------------------------------------------------------------------------}

-- | Pattern match on 'HasField'
--
-- This is intended to be used together with 'matchHasField'. Example usage:
--
-- > data Foo a
-- >
-- > instance HasField "fooX" (Foo a) Int where ..
-- > instance HasField "fooY" (Foo a) [a] where ..
-- >
-- > _example :: Foo Char -> (Int, [Char])
-- > _example (matchHasField -> ( fieldNamed @"fooX" -> x
-- >                            , fieldNamed @"fooY" -> y
-- >                            ) ) = (x, y)
class MatchHasField a b | b -> a where
  matchHasField :: a -> b

-- | To be used in conjunction with 'MatchHasField'.
--
-- See 'MatchHasField' for details.
fieldNamed :: forall x r a. GetField x r a -> a
fieldNamed (GetField a) = a

data GetField (x :: Symbol) (r :: Type) (a :: Type) = GetField a

instance HasField x r a => MatchHasField r (GetField x r a) where
  matchHasField = GetField . (getField @x)

instance (MatchHasField a b, MatchHasField a c) => MatchHasField a (b, c) where
  matchHasField r = (matchHasField r, matchHasField r)

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

data Foo a

instance HasField "fooX" (Foo a) Int where hasField = undefined
instance HasField "fooY" (Foo a) [a] where hasField = undefined

_example :: Foo Char -> (Int, [Char])
_example (matchHasField -> ( fieldNamed @"fooX" -> x
                           , fieldNamed @"fooY" -> y
                           ) ) = (x, y)
