{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Generic conversion to/from JSON
module Data.Record.Generic.JSON (
    gtoJSON
  , gparseJSON
  ) where

import Data.Aeson
import Data.Aeson.Types
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

gparseJSON :: forall a. (Generic a, Constraints a FromJSON) => Value -> Parser a
gparseJSON = withObject recordName (fmap to . Rep.sequenceA . aux)
  where
    Metadata{..} = metadata (Proxy @a)

    aux :: Object -> Rep (Parser :.: I) a
    aux obj =
        Rep.cmap
          (Proxy @FromJSON)
          (\(K fld) -> Comp (I <$> getField fld))
          recordFieldNames
      where
        getField :: FromJSON x => String -> Parser x
        getField fld = obj .: Text.pack fld
