{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Record.Internal.RecordInfo (
    -- * Definitions
    RecordInfo(..)
  , FieldInfo(..)
    -- * Conversion
  , fromRecordDef
    -- * Combinators
  , setRecordInfoValues
  ) where

import Control.Monad.State
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Void
import Language.Haskell.TH.Syntax (Quasi, NameSpace(..))

import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Language.Haskell.TH as TH

import Data.Record.Internal.RecordDef

import qualified Data.Record.Internal.TH.Name as N

{-------------------------------------------------------------------------------
  Definitions
-------------------------------------------------------------------------------}

data RecordInfo a = RecordInfo {
      recordInfoUnqual :: N.Name 'TcClsName 'N.Global
    , recordInfoTVars  :: [TH.TyVarBndr]
    , recordInfoConstr :: N.Name 'DataName 'N.Global
    , recordInfoFields :: [FieldInfo a]
    }
  deriving (Show)

data FieldInfo a = FieldInfo {
      fieldInfoUnqual :: N.OverloadedName
    , fieldInfoType   :: TH.Type
    , fieldInfoIndex  :: Int
    , fieldInfoVal    :: Maybe a -- ^ Nothing if not present
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromRecordDef :: Quasi m => RecordDef -> m (RecordInfo Void)
fromRecordDef RecordDef{..} = do
    unqual <- N.deriveUnique recordDefUnqual
    constr <- N.deriveUnique recordDefConstr
    return RecordInfo{
        recordInfoUnqual = unqual
      , recordInfoTVars  = recordDefTVars
      , recordInfoConstr = constr
      , recordInfoFields = map fromFieldDef recordDefFields
      }

fromFieldDef :: FieldDef -> FieldInfo Void
fromFieldDef FieldDef{..} = FieldInfo{
      fieldInfoUnqual = fieldDefUnqual
    , fieldInfoType   = fieldDefType
    , fieldInfoIndex  = fieldDefIndex
    , fieldInfoVal    = Nothing
    }

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

setRecordInfoValues :: forall a.
     [(N.OverloadedName, a)]
  -> RecordInfo Void
  -> (RecordInfo a, [N.OverloadedName])
setRecordInfoValues values r = (
      r { recordInfoFields = sortBy (comparing fieldInfoIndex) $
                               Map.elems matched
        }
    , unknown
    )
  where
    given :: Map N.OverloadedName a
    given = Map.fromList values

    defined :: Map N.OverloadedName (FieldInfo Void)
    defined = Map.fromList $
                map (\f -> (fieldInfoUnqual f, f)) (recordInfoFields r)

    matched :: Map N.OverloadedName (FieldInfo a)
    unknown :: [N.OverloadedName]
    (matched, unknown) = flip runState [] $
        Map.mergeA
          (Map.traverseMissing      fieldMissing)
          (Map.traverseMaybeMissing fieldUnknown)
          (Map.zipWithAMatched      fieldPresent)
          defined
          given

    fieldPresent :: N.OverloadedName -> FieldInfo Void -> a -> State [N.OverloadedName]        (FieldInfo a)
    fieldMissing :: N.OverloadedName -> FieldInfo Void      -> State [N.OverloadedName]        (FieldInfo a)
    fieldUnknown :: N.OverloadedName ->                   a -> State [N.OverloadedName] (Maybe (FieldInfo a))

    fieldPresent _nm  f  a = return $ f { fieldInfoVal = Just a  }
    fieldMissing _nm  f    = return $ f { fieldInfoVal = Nothing }
    fieldUnknown  nm    _a = modify (nm:) >> return Nothing
