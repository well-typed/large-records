{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions to support the TH code (i.e., functions called by generated code)
--
-- NOTE: We leave the generic representation type as lazy, and only force
-- values once we translate back to the type itself. This means that we can
-- chain generic functions and get some kind of fusion without having to
-- traverse records multiple times.
module Data.Record.TH.Runtime (
    -- * Miscellaneous
    dictFor
  , repFromVector
  , repToVector
  , rnfVectorAny
  ) where

import Data.Coerce (coerce)
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Any)

import qualified Data.Vector as V

import Data.Record.Generic

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

dictFor :: c x => Proxy c -> Proxy x -> Dict c x
dictFor _ _ = Dict

repFromVector :: Vector Any -> Rep I a
repFromVector = coerce

repToVector :: Rep I a -> Vector Any
repToVector = coerce

rnfVectorAny :: Vector Any -> ()
rnfVectorAny = rnfElems . V.toList
  where
    rnfElems :: [Any] -> ()
    rnfElems []     = ()
    rnfElems (x:xs) = x `seq` rnfElems xs

