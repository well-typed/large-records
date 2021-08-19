{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Record.Internal.Record (
    -- * Record description
    Record(..)
  , Field(..)
    -- * Combinators
  , matchRecordFields
  , dropMissingRecordFields
  ) where

import Control.Monad.State
import Data.List (sortBy)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Language.Haskell.TH

import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map

{-------------------------------------------------------------------------------
  Description
-------------------------------------------------------------------------------}

-- | Record description
data Record a = Record {
      -- | Record type name
      recordType :: String

      -- | Record constructor name
    , recordConstr :: String

      -- | Type variables in the records type
    , recordTVars :: [TyVarBndr]

      -- | Fields in the record
    , recordFields :: [Field a]
    }
  deriving stock (Show, Functor, Foldable, Traversable)

-- | Record field description
data Field a = Field {
      -- | Field name
      fieldName :: String

      -- | Type of the field
    , fieldType :: Type

      -- | Index of the field (field 0, field 1, ..)
      --
      -- This is strictly speaking redundant information, as this is already
      -- implied by the position of the field in 'recordFields'. However, since
      -- we do a lot of positional processing (every field corresponds to a
      -- vector element), it is convenient to have the index readily available.
    , fieldIndex :: Int

      -- | Value associated with this field ('Nothing' if not present)
    , fieldVal :: a
    }
  deriving stock (Show, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Match field values against field definitions
--
-- We explicit mark missing fields; one use case for this is record
-- construction, where we want to issue a warning for missing fields.
matchRecordFields :: forall a b.
     [(String, b)]
  -> Record a
  -> (Record (a, Maybe b), [String])
matchRecordFields values r = (
      r { recordFields = sortBy (comparing fieldIndex) $
                               Map.elems matched
        }
    , unknown
    )
  where
    given :: Map String b
    given = Map.fromList values

    defined :: Map String (Field a)
    defined = Map.fromList $
                map (\f -> (fieldName f, f)) (recordFields r)

    matched :: Map String (Field (a, Maybe b))
    unknown :: [String]
    (matched, unknown) = flip runState [] $
        Map.mergeA
          (Map.traverseMissing      fieldMissing)
          (Map.traverseMaybeMissing fieldUnknown)
          (Map.zipWithAMatched      fieldPresent)
          defined
          given

    fieldPresent :: String -> Field a -> b -> State [String]        (Field (a, Maybe b))
    fieldMissing :: String -> Field a      -> State [String]        (Field (a, Maybe b))
    fieldUnknown :: String ->            b -> State [String] (Maybe (Field (a, Maybe b)))

    fieldPresent _nm  f  a = return $ f { fieldVal = (fieldVal f, Just a) }
    fieldMissing _nm  f    = return $ f { fieldVal = (fieldVal f, Nothing) }
    fieldUnknown  nm    _a = modify (nm:) >> return Nothing

dropMissingRecordFields :: Record (Maybe a) -> Record a
dropMissingRecordFields r =
    r { recordFields = mapMaybe sequenceA (recordFields r) }
