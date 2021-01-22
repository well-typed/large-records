{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Record.Generic.Size.Infra (
    recordOfSize
  ) where

import GHC.TypeLits (Nat)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

{-------------------------------------------------------------------------------
  Construct records of specified size
-------------------------------------------------------------------------------}

recordOfSize :: Integer -> Q [Dec]
recordOfSize n = fmap (:[]) $
    dataD
      (cxt [])
      (mkName $ "R" ++ show n)
      []
      Nothing
      [recC (mkName $ "MkR" ++ show n) fields]
      []
  where
    fields :: [Q VarBangType]
    fields = [
          varBangType
            (mkName $ "field" ++ show i)
            (bangType defaultBang (conT ''T `appT` litT (numTyLit i)))
        | i <- [1 .. n]
        ]

    defaultBang :: BangQ
    defaultBang = bang noSourceUnpackedness noSourceStrictness

{-------------------------------------------------------------------------------
  Definitions referred to by the generated TH code
-------------------------------------------------------------------------------}

-- | 'T' gives us as many different types as we need
newtype T (i :: Nat) = MkT Int
