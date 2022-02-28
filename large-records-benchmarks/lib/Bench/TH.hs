{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Bench.TH (
    recordOfSize
  , higherKindedRecordOfSize
  ) where

import Data.Kind

import Language.Haskell.TH hiding (Type, TyVarBndr(..))

#if !MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax hiding (Type, TyVarBndr(..))
#endif

import Bench.Types
import Bench.TH.Compat

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
    f     <- newName "field"
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
          do fieldI <- newName $ "f" ++ show i
             varBangType
               fieldI
               (bangType defaultBang (conT ''HK `appT` litT (numTyLit i) `appT` varT f))
        | i <- [1 .. n]
        ]

defaultBang :: BangQ
defaultBang = bang noSourceUnpackedness noSourceStrictness

