{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic conversion to/from JSON
module Data.Record.Generic.JSON (
    gtoJSON
  ) where

import Data.Aeson
import Data.Proxy

import Data.Record.Generic
import qualified Data.Record.Generic.Rep as Rep

gtoJSON :: forall a. (Metadata a, Constraints ToJSON a) => a -> Value
gtoJSON =
      object
    . Rep.collapse
    . Rep.zipWith (mapKKK (,)) (metadata (Proxy @a))
    . Rep.cmap (Proxy @ToJSON) (K . toJSON . unI)
    . from
