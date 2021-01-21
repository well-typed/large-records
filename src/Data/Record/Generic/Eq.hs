{-# LANGUAGE TypeApplications #-}

module Data.Record.Generic.Eq (
    geq
  ) where

import Data.Record.Generic
import qualified Data.Record.Generic.Rep as Rep

-- | Generic equality function
--
-- Typical usage:
--
-- > instance Eq T where
-- >   (==) = geq
--
-- TODO: Should we worry about short-circuiting here?
geq :: (Generic a, Constraints a Eq) => a -> a -> Bool
geq = \x y ->
      and
    . Rep.collapse
    $ Rep.czipWith (Proxy @Eq) compareField (from x) (from y)
  where
    compareField :: Eq x => I x -> I x -> K Bool x
    compareField (I x) (I y) = K (x == y)
