{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TypeOperators  #-}

module Bench.HList (
    HList(..)
    -- * Conversions
  , hlistToListAny
  , SomeHList(..)
  , hlistFromListAny
  , hlistToNP
  , hlistFromNP
    -- * Examples
  , exampleRec0
  , exampleRec1
  , exampleRec2
  , exampleRec3
  , exampleRec4
  , exampleRec5
  ) where

import Data.SOP.BasicFunctors
import Data.SOP.NP (NP)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.SOP.NP as NP

{-------------------------------------------------------------------------------
  Basic definition
-------------------------------------------------------------------------------}

data HList xs where
  Nil  :: HList '[]
  (:*) :: x -> HList xs -> HList (x ': xs)

infixr 5 :*

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

hlistToListAny :: HList xs -> [I Any]
hlistToListAny Nil       = []
hlistToListAny (x :* xs) = unsafeCoerce x : hlistToListAny xs

data SomeHList where
  SomeHList :: HList xs -> SomeHList

hlistFromListAny :: [I Any] -> SomeHList
hlistFromListAny []     = SomeHList Nil
hlistFromListAny (x:xs) = case hlistFromListAny xs of
                            SomeHList xs' -> SomeHList (x :* xs')

hlistToNP :: HList xs -> NP I xs
hlistToNP Nil       = NP.Nil
hlistToNP (x :* xs) = I x NP.:* hlistToNP xs

hlistFromNP :: NP I xs -> HList xs
hlistFromNP NP.Nil         = Nil
hlistFromNP (I x NP.:* xs) = x :* hlistFromNP xs

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- | Example record with 0 fields
exampleRec0 :: HList '[]
exampleRec0 = Nil

-- | Example record with 1 field
exampleRec1 :: HList '[Int]
exampleRec1 = 1234 :* Nil

-- | Example record with 2 fields
exampleRec2 :: HList '[Int, Char]
exampleRec2 = 1234 :* 'x' :* Nil

-- | Example record with 3 fields
exampleRec3 :: HList '[Int, Char, String]
exampleRec3 = 1234 :* 'x' :* "yz" :* Nil

-- | Example record with 4 fields
exampleRec4 :: HList '[Int, Char, String, Double]
exampleRec4 = 1234 :* 'x' :* "yz" :* 5.6 :* Nil

-- | Example record with 5 fields
exampleRec5 :: HList '[Int, Char, String, Double, Bool]
exampleRec5 = 1234 :* 'x' :* "yz" :* 5.6 :* True :* Nil
