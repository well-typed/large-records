{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Record.Beam.Util.Orphans () where

import Database.Beam.Schema.Tables

deriving instance Eq   (Ignored x)
deriving instance Show (Ignored x)

deriving instance Show (DatabaseEntityDescriptor be entityType) => Show (DatabaseEntity be db entityType)
deriving instance Eq   (DatabaseEntityDescriptor be entityType) => Eq   (DatabaseEntity be db entityType)

deriving instance Show (TableSettings tbl) => Show (DatabaseEntityDescriptor be (TableEntity tbl))
deriving instance Show (TableSettings tbl) => Show (DatabaseEntityDescriptor be (ViewEntity tbl))

deriving instance Eq (TableSettings tbl) => Eq (DatabaseEntityDescriptor be (TableEntity tbl))
deriving instance Eq (TableSettings tbl) => Eq (DatabaseEntityDescriptor be (ViewEntity tbl))

deriving instance Show (DatabaseEntityDescriptor be (DomainTypeEntity a))
deriving instance Eq   (DatabaseEntityDescriptor be (DomainTypeEntity a))
