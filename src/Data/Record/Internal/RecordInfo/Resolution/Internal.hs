{-# LANGUAGE DataKinds #-}

module Data.Record.Internal.RecordInfo.Resolution.Internal (
    getRecordInfo
  , putRecordInfo
  ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Void
import Language.Haskell.TH.Syntax

import qualified Data.Map as Map

import Data.Record.Internal.RecordInfo

import qualified Data.Record.Internal.TH.Name as N

{-------------------------------------------------------------------------------
  Internal state
-------------------------------------------------------------------------------}

newtype TypeEnv = WrapTypeEnv {
      unwrapTypeEnv :: Map (N.Name 'N.DataName 'N.Global) (RecordInfo Void)
    }

getTypeEnv :: Quasi m => m TypeEnv
getTypeEnv = fromMaybe (WrapTypeEnv Map.empty) <$> qGetQ

putTypeEnv :: Quasi m => TypeEnv -> m ()
putTypeEnv = qPutQ

{-------------------------------------------------------------------------------
  Accessing the internal state
-------------------------------------------------------------------------------}

getRecordInfo ::
     Quasi m
  => N.Name 'N.DataName 'N.Global -> m (Maybe (RecordInfo Void))
getRecordInfo constr =  Map.lookup constr . unwrapTypeEnv <$> getTypeEnv

putRecordInfo ::
     Quasi m
  => RecordInfo Void -> m ()
putRecordInfo info = do
    env <- unwrapTypeEnv <$> getTypeEnv
    putTypeEnv $ WrapTypeEnv $ Map.insert constr info env
  where
    constr :: N.Name 'DataName 'N.Global
    constr = recordInfoConstr info
