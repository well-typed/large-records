{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Infrastructure for promoting terms to types and back
--
-- TODO: Should this live in tests?
-- TODO: Should we use standard singletons infra?
module Data.Record.Anonymous.Util.Promotion (
    Promoted
  , Bounce(..)
  ) where

import Data.Kind
import GHC.TypeLits

import Data.Record.Anonymous.Util.Singleton

{-------------------------------------------------------------------------------
  Main definitions
-------------------------------------------------------------------------------}

-- | Promote a type to a kind
--
-- NOTE: This establishes a link between a /type/ @a ::Type@ and a corresponding
-- kind. Specifically, this can /not/ (and does not need to) be used for types
-- have already been promoted. For example, consider
--
-- > data T :: Bool -> Type -> Type where
-- >   TA :: a -> T True  a
-- >   TB :: a -> T False a
--
-- The appropriate 'Promoted' instance is
--
-- > type instance Promoted (T b a) = T b (Promoted a)
--
-- The boolean /already/ lives at the type-level and so does not (cannot) be
-- promoted; this would be a kind error:
--
-- type instance Promoted (T b a) = T (Promoted b) (Promoted a) -- Incorrect
--
-- because @b@ does not have kind 'Type'.
type family Promoted (a :: Type) :: Type

-- | Establish a link between the term and the type level
class Bounce a where
  promote :: a -> SomeSing (Promoted a)
  demote  :: Sing (x :: Promoted a) -> a

{-------------------------------------------------------------------------------
  Standard instances
-------------------------------------------------------------------------------}

type instance Promoted String    = Symbol
type instance Promoted Integer   = Nat
type instance Promoted (Maybe a) = Maybe (Promoted a)
type instance Promoted (a, b)    = (Promoted a, Promoted b)

instance Bounce String where
  promote str = case someSymbolVal str of
                  SomeSymbol p -> SomeSing $ SSymbol p

  demote (SSymbol p) = symbolVal p

instance Bounce Integer where
  promote n = case someNatVal n of
                Just (SomeNat p) -> SomeSing $ SNat p
                Nothing          -> error $ "promote: integer out of range"

  demote (SNat p) = natVal p

instance Bounce a => Bounce (Maybe a) where
  promote Nothing  = SomeSing $ SNothing
  promote (Just x) = case promote x of
                       SomeSing x' -> SomeSing $ SJust x'

  demote SNothing  = Nothing
  demote (SJust x) = Just (demote x)

instance (Bounce a, Bounce b) => Bounce (a, b) where
  promote (a, b) = case (promote a, promote b) of
                     (SomeSing a', SomeSing b') -> SomeSing $ SPair a' b'

  demote (SPair a b) = (demote a, demote b)
