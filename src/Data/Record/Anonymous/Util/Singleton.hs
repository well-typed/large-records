{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Standard singletons
--
-- TODO: Should this live in tests?
-- TODO: Should we use standard singletons infra?
module Data.Record.Anonymous.Util.Singleton (
    -- * Standard singletons
    Sing(..)
  , SomeSing(..)
    -- * Axioms
  , axiomCmpSymbol
  ) where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce

{-------------------------------------------------------------------------------
  Standard singletons
-------------------------------------------------------------------------------}

data family Sing :: k -> Type

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

{-------------------------------------------------------------------------------
  Instances

  TODO: This isn't /quite/ a singleton (see instance for Type). Rename?
-------------------------------------------------------------------------------}

newtype instance Sing :: Type -> Type where
  SVal :: a -> Sing a

data instance Sing :: Ordering -> Type where
  SLT :: Sing 'LT
  SGT :: Sing 'GT
  SEQ :: Sing 'EQ

data instance Sing :: Symbol -> Type where
  SSymbol :: KnownSymbol s => Proxy s -> Sing (s :: Symbol)

data instance Sing :: Nat -> Type where
  SNat :: KnownNat n => Proxy n -> Sing (n :: Nat)

data instance Sing :: Maybe a -> Type where
  SNothing :: Sing ('Nothing :: Maybe a)
  SJust    :: Sing (x :: a) -> Sing ('Just x :: Maybe a)

data instance Sing :: (a, b) -> Type where
  SPair :: Sing (x :: a) -> Sing (y :: b) -> Sing ('(x, y) :: (a, b))

{-------------------------------------------------------------------------------
  Axioms
-------------------------------------------------------------------------------}

axiomCmpSymbol ::
     Sing (m :: Symbol)
  -> Sing (n :: Symbol)
  -> Sing (CmpSymbol m n :: Ordering)
axiomCmpSymbol (SSymbol m) (SSymbol n) =
    case compare (symbolVal m) (symbolVal n) of
      LT -> unsafeCoerce SLT
      GT -> unsafeCoerce SGT
      EQ -> unsafeCoerce SEQ

