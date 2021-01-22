{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}

module Test.Record.Generic.Size.Infra (
    recordOfSize
    -- Supporting infrastructure used by generated code
  , T(..)
  ) where

import Data.Aeson
import GHC.TypeLits (Nat)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Record.Generic.LowerBound

{-------------------------------------------------------------------------------
  Construct records of specified size
-------------------------------------------------------------------------------}

recordOfSize :: Integer -> Q [Dec]
recordOfSize n = fmap (:[]) $
    dataD
      (cxt [])
      (mkName "R")
      []
      Nothing
      [recC (mkName "MkR") fields]
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
newtype T (i :: Nat) = MkT Word
  deriving (Show, Eq, ToJSON, LowerBound)
