{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Generic conversion to/from JSON
module Data.Record.Generic.JSON (
    gtoJSON
  ) where

import Data.Aeson
import Data.Proxy

import qualified Data.Text as Text

import Data.Record.Generic
import qualified Data.Record.Generic.Rep as Rep

gtoJSON :: forall a. (Generic a, Constraints a ToJSON) => a -> Value
gtoJSON =
      object
    . Rep.collapse
    . Rep.zipWith (mapKKK $ \n x -> (Text.pack n, x)) recordFieldNames
    . Rep.cmap (Proxy @ToJSON) (K . toJSON . unI)
    . from
  where
    Metadata{..} = metadata (Proxy @a)
