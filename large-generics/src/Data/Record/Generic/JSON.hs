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
import Data.String

import Data.Record.Generic
import qualified Data.Record.Generic.Rep as Rep

gtoJSON :: forall a. (Generic a, Constraints a ToJSON) => a -> Value
gtoJSON =
      object
    . Rep.collapse
    . Rep.zipWith (mapKKK $ \n x -> (fromString n, x)) (recordFieldNames md)
    . Rep.cmap (Proxy @ToJSON) (K . toJSON . unI)
    . from
  where
    md = metadata (Proxy @a)

gparseJSON :: forall a. (Generic a, Constraints a FromJSON) => Value -> Parser a
gparseJSON =
    withObject (recordName md) (fmap to . Rep.sequenceA . aux)
  where
    md = metadata (Proxy @a)

    aux :: Object -> Rep (Parser :.: I) a
    aux obj =
        Rep.cmap
          (Proxy @FromJSON)
          (\(K fld) -> Comp (I <$> getField fld))
          (recordFieldNames md)
      where
        getField :: FromJSON x => String -> Parser x
        getField fld = obj .: fromString fld
