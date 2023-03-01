{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Record.Beam.DbSettings (
    GAutoLargeDbSettings
  , DbSettingsI
  ) where

import Data.Record.Generic
import Data.Record.Generic.GHC
import Data.Record.Generic.Transform
import Database.Beam.Schema.Tables
import GHC.Generics hiding (Generic(..))

import qualified Data.Record.Generic.Rep as Rep

import Data.Record.Beam.Interpretation

{-------------------------------------------------------------------------------
  DB settings
-------------------------------------------------------------------------------}

type GAutoLargeDbSettings be db = (
    Generic (db Uninterpreted)
  , Generic (db (DatabaseEntity be db))
  , HasNormalForm (DefaultInterpretation (DatabaseEntity be db)) (db (DatabaseEntity be db)) (db Uninterpreted)
  , Constraints (db Uninterpreted) (DbSettingsI be db)
  )

instance GAutoLargeDbSettings be db
      => (GAutoDbSettings (ThroughLRGenerics (db (DatabaseEntity be db)) ())) where
  autoDbSettings' = WrapThroughLRGenerics $
      to . denormalize1 (Proxy @DefaultInterpretation) $
        Rep.cmap
          (Proxy @(DbSettingsI be db))
          dbSettingsI
          (ghcMetadataFields (ghcMetadata (Proxy @(db Uninterpreted))))

class DbSettingsI be db x where
  dbSettingsI ::
       GhcFieldMetadata x
    -> Interpret (DefaultInterpretation (DatabaseEntity be db)) x

instance (IsDatabaseEntity be x, DatabaseEntityDefaultRequirements be x)
      => DbSettingsI be db (Uninterpreted x) where
  dbSettingsI (GhcFieldMetadata p) = Interpret $ unK1 . unM1 $ fromBeam p
    where
      fromBeam ::
           Selector f
        => Proxy f
        -> S1 f (K1 R (DatabaseEntity be db x)) p
      fromBeam _ = autoDbSettings'

{-------------------------------------------------------------------------------
  Table settings
-------------------------------------------------------------------------------}

type GDefaultLargeTableFieldSettings tbl sub = (
    Generic (tbl (TableField sub))
  , Generic (tbl Uninterpreted)
  , HasNormalForm (BeamInterpretation (TableField sub)) (tbl (TableField sub)) (tbl Uninterpreted)
  , Constraints (tbl Uninterpreted) (TableSettingsI sub)
  )

instance GDefaultLargeTableFieldSettings tbl sub
      => GDefaultTableFieldSettings (ThroughLRGenerics (tbl (TableField sub)) ())
  where
    gDefTblFieldSettings _ = WrapThroughLRGenerics $
        to . denormalize1 (Proxy @BeamInterpretation) $
          Rep.cmap
            (Proxy @(TableSettingsI sub))
            tableSettingsI
            (ghcMetadataFields (ghcMetadata (Proxy @(tbl Uninterpreted))))

class TableSettingsI tbl x where
  tableSettingsI ::
       GhcFieldMetadata x
    -> Interpret (BeamInterpretation (TableField tbl)) x

instance TableSettingsI tbl (Uninterpreted x) where
  tableSettingsI (GhcFieldMetadata p) = Interpret $ unK1 . unM1 $ fromBeam p
    where
      fromBeam :: Selector f => Proxy f -> S1 f (K1 R (TableField tbl x)) p
      fromBeam _ = gDefTblFieldSettings Proxy

instance ( ChooseSubTableStrategy tbl sub ~ strategy
         , SubTableStrategyImpl strategy (TableField tbl) sub
         , Beamable sub
         ) => TableSettingsI tbl (sub Uninterpreted) where
  tableSettingsI (GhcFieldMetadata p) = Interpret $ unK1 . unM1 $ fromBeam p
    where
      fromBeam :: Selector f => Proxy f -> S1 f (K1 R (sub (TableField tbl))) p
      fromBeam _ = gDefTblFieldSettings Proxy

instance ( ChooseSubTableStrategy tbl sub ~ strategy
         , SubTableStrategyImpl strategy (Nullable (TableField tbl)) sub
         , Beamable sub
         ) => TableSettingsI tbl (sub (Nullable Uninterpreted)) where
  tableSettingsI (GhcFieldMetadata p) = Interpret $ unK1 . unM1 $ fromBeam p
    where
      fromBeam :: Selector f => Proxy f -> S1 f (K1 R (sub (Nullable (TableField tbl)))) p
      fromBeam _ = gDefTblFieldSettings Proxy

