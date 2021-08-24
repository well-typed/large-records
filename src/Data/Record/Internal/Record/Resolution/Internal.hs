{-# LANGUAGE DataKinds #-}

module Data.Record.Internal.Record.Resolution.Internal (
    getRecordInfo
  , putRecordInfo
  ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax

import qualified Data.Map as Map

import Data.Record.Internal.Naming
import Data.Record.Internal.Record
import Data.Record.TH.Config.Options (GenPatSynonym(..))

import qualified Data.Record.Internal.TH.Name as N

{-------------------------------------------------------------------------------
  Internal state

  As keys we use the names of the internal constructor, because when we do name
  resolution, that is what we would normally use to query ghc. We use /global/
  names, which uniquely identify a name (qualified by package and module).
-------------------------------------------------------------------------------}

newtype TypeEnv = WrapTypeEnv {
      unwrapTypeEnv :: Map (N.Name 'DataName 'N.Global) (Record ())
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
  => N.Name 'DataName 'N.Global  -- ^ Name of the internal constructor
  -> m (Maybe (Record ()))
getRecordInfo internalConstr =
    Map.lookup internalConstr . unwrapTypeEnv <$> getTypeEnv

-- | Add 'RecordInfo' to the environment
--
-- NOTE: Must be called whilst processing the module in which the record is
-- defined.
putRecordInfo :: Quasi m => Record () -> m ()
putRecordInfo info = do
    env <- unwrapTypeEnv <$> getTypeEnv

    -- In order to be able to resolve the record info later, we need to properly
    -- quantify the record name. We do this by requesting the /current/ TH
    -- location. This is justified by the precondition to the function.
    --
    -- Moreover, since only the quasi-quoter will do this resolution, the name
    -- we use as a key in the environment is the name that the quasi-quoter
    -- would use.

    loc <- runQ location
    let internalConstr :: N.Name 'DataName 'N.Global
        internalConstr =
          N.Name
            (OccName (nameRecordInternalConstr UseQuasiQuoter (recordConstr info)))
            (N.NameGlobal
              DataName
              (mkPkgName (loc_package loc))
              (mkModName (loc_module  loc))
            )

    putTypeEnv $ WrapTypeEnv $ Map.insert internalConstr info env

