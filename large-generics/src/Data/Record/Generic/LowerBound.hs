{-# LANGUAGE TypeApplications #-}

-- | Simple example of a generic function
module Data.Record.Generic.LowerBound (
    LowerBound(..)
  , glowerBound
  ) where

import Data.Record.Generic
import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  General definition
-------------------------------------------------------------------------------}

-- | Types with a lower bound
class LowerBound a where
  lowerBound :: a

instance LowerBound Word where lowerBound = 0
instance LowerBound Bool where lowerBound = False
instance LowerBound Char where lowerBound = '\x0000'
instance LowerBound ()   where lowerBound = ()
instance LowerBound [a]  where lowerBound = []

{-------------------------------------------------------------------------------
  Generic definition
-------------------------------------------------------------------------------}

glowerBound :: (Generic a, Constraints a LowerBound) => a
glowerBound = to $ Rep.cpure (Proxy @LowerBound) (I lowerBound)
