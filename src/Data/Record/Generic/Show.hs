{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Record.Generic.Show (
    gshowsPrec
  ) where

import Data.Record.Generic
import Data.List (intersperse)
import GHC.Show

import qualified Data.Record.Generic.Rep as Rep

-- | Generic definition of 'showsPrec', compatible with the GHC generated one.
--
-- Typical usage:
--
-- > instance Show T where
-- >   showsPrec = gshowsPrec
gshowsPrec :: forall a. (Generic a, Constraints a Show) => Int -> a -> ShowS
gshowsPrec d =
      aux
    . Rep.collapse
    . Rep.czipWith (Proxy @Show) showField (recordFieldNames md)
    . from
  where
    md = metadata (Proxy @a)

    showField :: Show x => K String x -> I x -> K ShowS x
    showField (K n) (I x) = K $ showString n . showString " = " . showsPrec 0 x

    aux :: [ShowS] -> ShowS
    aux fields = showParen (d >= 11) (
          showString (recordConstructor md) . showString " {"
        . foldr (.) id (intersperse showCommaSpace fields)
        . showString "}"
        )
