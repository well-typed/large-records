{-# LANGUAGE RecordWildCards #-}

-- | Names used in code generation
--
-- Intended for unqualified.
module Data.Record.Anon.Internal.Plugin.Source.Names (
    -- * large-anon
    LargeAnonNames(..)
  , getLargeAnonNames
  ) where

import Data.Record.Anon.Internal.Plugin.Source.GhcShim
import Data.Record.Anon.Internal.Plugin.Source.Options (Mode(..))

{-------------------------------------------------------------------------------
  large-anon
-------------------------------------------------------------------------------}

-- | Named required for code generation
--
-- All names are expected to be exact.
data LargeAnonNames = LargeAnonNames {
      largeAnon_empty        :: RdrName
    , largeAnon_insert       :: RdrName
    , largeAnon_applyPending :: RdrName
    , largeAnon_letRecordT   :: RdrName
    , largeAnon_letInsertAs  :: RdrName
    , typelet_castEqual      :: RdrName
    }

getLargeAnonNames :: Mode -> Hsc LargeAnonNames
getLargeAnonNames mode = do
    -- We avoid importing names from other packages; see detailed discussion
    -- in "Data.Record.Anon.Plugin.Internal.Runtime".
    largeAnon_empty        <- Exact <$> lookupName modl Nothing "empty"
    largeAnon_insert       <- Exact <$> lookupName modl Nothing "insert"
    largeAnon_applyPending <- Exact <$> lookupName modl Nothing "applyPending"
    largeAnon_letRecordT   <- Exact <$> lookupName modl Nothing "letRecordT"
    largeAnon_letInsertAs  <- Exact <$> lookupName modl Nothing "letInsertAs"
    typelet_castEqual      <- Exact <$> lookupName modl Nothing "castEqual"
    return LargeAnonNames{..}
  where
    modl :: ModuleName
    modl = case mode of
             Simple   -> mkModuleName "Data.Record.Anon.Simple"
             Advanced -> mkModuleName "Data.Record.Anon.Advanced"
