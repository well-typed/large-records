{-# LANGUAGE TypeApplications #-}

module Data.Record.Generic.NFData (
    grnf
  ) where

import Control.DeepSeq (NFData, rnf)
import Data.Record.Generic
import qualified Data.Record.Generic.Rep as Rep

-- | Generic rnf function
--
-- Typical usage:
--
-- > instance NFData T where
-- >   rnf = grnf
--
grnf :: (Generic a, Constraints a NFData) => a -> ()
grnf =
    rnf
  . Rep.collapse
  . Rep.cmap (Proxy @NFData) (mapIK rnf)
  . from
