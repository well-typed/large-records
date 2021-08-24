{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

-- | Infrastructure for supporting matching on records
--
-- We are be careful not to reintroduce quadratic code size here.
module Data.Record.QQ.Runtime.MatchHasField (
    MatchHasField -- opaque
  , matchHasField
  , fieldNamed
  , viewAtType
  ) where

import Data.Kind
import GHC.Records.Compat
import GHC.TypeLits

{-------------------------------------------------------------------------------
  Infrastructure
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
fieldNamed :: GetField x r a -> a
fieldNamed (GetField a) = a

data GetField (x :: Symbol) (r :: Type) (a :: Type) = GetField a

instance HasField x r a => MatchHasField r (GetField x r a) where
  matchHasField = GetField . (getField @x)

instance (MatchHasField a b, MatchHasField a c) => MatchHasField a (b, c) where
  matchHasField r = (matchHasField r, matchHasField r)

-- | Can be used alongside 'matchHasField' to fix the type of the argument
--
-- This avoids inferring types in terms of @HasField ..@; see example below.
--
-- The first argument will usually be instantiated to some form of @undefined@;
-- we use it instead of a proxy to avoid difficulties with type variables.
viewAtType :: a -> a -> a
viewAtType ~_proxy = id

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

data Foo a = MkFoo

instance HasField "fooX" (Foo a) Int where hasField = undefined
instance HasField "fooY" (Foo a) [a] where hasField = undefined

_example1 :: (HasField "fooX" a b, HasField "fooY" a c) => a -> (b, c)
_example1 (matchHasField -> ( fieldNamed @"fooX" -> x
                            , fieldNamed @"fooY" -> y
                            ) ) = (x, y)

_example2 :: Foo a -> (Int, [a]) -- This is the inferred signature
_example2 (viewAtType MkFoo -> matchHasField -> ( fieldNamed @"fooX" -> x
                                                , fieldNamed @"fooY" -> y
                                                ) ) = (x, y)
