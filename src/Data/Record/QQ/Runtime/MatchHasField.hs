{-# LANGUAGE DataKinds              #-}
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
