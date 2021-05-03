{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Record.Size.Infra (
    recordOfSize
  , higherKindedRecordOfSize
    -- Supporting infrastructure used by generated code
  , T(..)
  ) where

import Data.Aeson
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import GHC.TypeLits

import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type)

import Data.Record.Generic.LowerBound

{-------------------------------------------------------------------------------
  Construct records of specified size
-------------------------------------------------------------------------------}

-- | Generate something like
--
-- > data R = MkR {
-- >       field1  :: T 1
-- >     , field2  :: T 2
-- >     , field3  :: T 3
-- >     , field4  :: T 4
-- >     , field5  :: T 5
-- >     }
--
-- In order to be compatible with the records originating from @[d| ... |]@,
-- we use 'newName' here rather than 'mkName'.
recordOfSize :: Integer -> Q [Dec]
recordOfSize n = fmap (:[]) $ do
    r   <- newName "R"
    mkR <- newName "MkR"
    dataD
      (cxt [])
      r
      []
      Nothing
      [recC mkR fields]
      []
  where
    fields :: [Q VarBangType]
    fields = [
          do fieldI <- newName $ "field" ++ show i
             varBangType
               fieldI
               (bangType defaultBang (conT ''T `appT` litT (numTyLit i)))
        | i <- [1 .. n]
        ]

-- | Like 'recordOfSize', but generate a record with a higher-kinded type var.
--
-- Generates something like:
--
-- > data HKR (f :: Type -> Type) = MkHKR {
-- >       field1  :: HK 1 f
-- >     , field2  :: HK 2 f
-- >     , field3  :: HK 3 f
-- >     , field4  :: HK 4 f
-- >     , field5  :: HK 5 f
-- >     }
-- >   deriving Show
--
-- See comments in 'recordOfSize' for the use of 'newName'.
higherKindedRecordOfSize :: Integer -> Q [Dec]
higherKindedRecordOfSize n = fmap (:[]) $ do
    f     <- newName "f"
    k     <- [t| Type -> Type |]
    hkr   <- newName "HKR"
    mkHKR <- newName "MkHKR"
    dataD
      (cxt [])
      hkr
      [KindedTV f k]
      Nothing
      [recC mkHKR (fields f)]
      [derivClause Nothing [conT ''Show]]
  where
    fields :: Name -> [Q VarBangType]
    fields f = [
          do fieldI <- newName $ "field" ++ show i
             varBangType
               fieldI
               (bangType defaultBang (conT ''HK `appT` litT (numTyLit i) `appT` varT f))
        | i <- [1 .. n]
        ]

defaultBang :: BangQ
defaultBang = bang noSourceUnpackedness noSourceStrictness

{-------------------------------------------------------------------------------
  Definitions referred to by the generated TH code
-------------------------------------------------------------------------------}

-- | 'T' gives us as many different types as we need
newtype T (i :: Nat) = MkT Word
  deriving (Show, Eq, ToJSON)

-- | Like 'T', but with a higher-kinded type variable
newtype HK (i :: Nat) (f :: Type -> Type) = MkHK (f Word)

instance KnownNat i => LowerBound (T i) where
  lowerBound = MkT $ fromInteger $ natVal (Proxy @i)

instance KnownNat i => LowerBound (HK i Identity) where
  lowerBound = MkHK $ fromInteger $ natVal (Proxy @i)

instance Show1 f => Show (HK i f) where
  showsPrec d (MkHK x) = showsPrec1 d x
