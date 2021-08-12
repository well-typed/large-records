{-# LANGUAGE DataKinds #-}

module Data.Record.Internal.RecordInfo.Resolution (
    resolveRecordInfo
  ) where

import Data.Void
import Language.Haskell.TH.Syntax

import Data.Record.Internal.RecordInfo

import qualified Data.Record.Internal.RecordInfo.Resolution.GHC as GHC
import qualified Data.Record.Internal.RecordInfo.Resolution.Internal as Internal
import qualified Data.Record.Internal.TH.Name as N

-- | Resolve record info
--
-- When the quasi-quoter needs to turn
--
-- > [lr| MkRecord { field2 = 5, field1 = True } |]
--
-- into
--
-- > _construct_MkRecord True 5
--
-- it needs to know the record definition: the types of all fields, and the
-- order of all fields.
--
-- The primary means through which we achieve this is by looking up the
-- 'MetadataOf' type family instance for the record, and then parsing that
-- ('GHC.parseRecordInfo').
--
-- Unfortunately, however, this does not always work. In an example such as
--
-- > largeRecord defaultPureScript [d|
-- >     data SomeRecord = MkRecord { field1 :: Int, field2 :: Bool }
-- >   |]
-- >
-- > foo :: SomeRecord
-- > foo = [lr| MkRecord { field1 = 5, field2 = True } |]
--
-- the call to 'largeRecord' and the definition of @foo@ is considered to be a
-- single binding group (not entirely sure why). Both the 'largeRecord' splice
-- and the 'lr' quasi-quote are now run /before typechecking/, which is why when
-- we get to the 'lr' quasi-quote, the 'MetadataOf' instance is not yet
-- available, even though it /has/ been generated.
--
-- One work-around is to insert an empty splice
--
-- > largeRecord defaultPureScript [d|
-- >     data SomeRecord = MkRecord { field1 :: Int, .. }
-- >   |]
-- >
-- > $(return [])
-- >
-- > foo :: SomeRecord
-- > foo = [lr| MkRecord { field1 = 5, .. } |]
--
-- That works (and we previously did that, giving that empty splice a name
-- @endOfBindingGroup@), but requiring this is bad for useability of the lib.
-- Most users probably don't know what binding groups even are, much less want
-- to think about the scope of each binding group: that's a task for @ghc@.
--
-- Therefore, in addition to being able to parse the 'MetadataOf' instance, we
-- also maintain our own environment, mapping constructor names to record info
-- (see 'Internal.getRecordInfo'). The 'largeRecord' splice adds entries into
-- this environment, and the 'lr' quasi-quoter consults this environment.
-- The contents of this environment are ephemeral, of course, and certainly not
-- stored as part of interface files, so this is merely a backup for when the
-- 'MetadataOf' information is not available.
resolveRecordInfo :: Quasi m
  => N.Name 'N.DataName 'N.Global
  -> m (Either String (RecordInfo Void))
resolveRecordInfo constr = do
    mInfo <- Internal.getRecordInfo constr
    case mInfo of
      Just info -> return $ Right info
      Nothing   -> GHC.parseRecordInfo constr

