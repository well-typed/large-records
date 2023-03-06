{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- Not a fan of @AllowAmbiguousTypes@, but no choice here, /must/ be enabled
-- to export a 'getField' of the correct type.
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Restore \"regular\" environment when using @RebindableSyntax@
--
-- The @RebindableSyntax@ extension is currently required when using
-- @OverloadedRecordUpdate@, but when using this extension, a number of
-- functions are suddenly no longer in scope that normally are. This module
-- restores those functions to their standard definition.
module Data.Record.Anon.Overloading (
    module Prelude
  , Control.Arrow.app
  , Control.Arrow.arr
  , Control.Arrow.first
  , Control.Arrow.loop
  , (Control.Arrow.>>>)
  , (Control.Arrow.|||)
  , Data.String.fromString
  , GHC.OverloadedLabels.fromLabel
  , GHC.Records.getField
    -- * New definitions
  , ifThenElse
  , setField
  ) where

import qualified Control.Arrow
import qualified Data.String
import qualified GHC.OverloadedLabels
import qualified GHC.Records
import qualified GHC.Records.Compat

{-------------------------------------------------------------------------------
  Other exports
-------------------------------------------------------------------------------}

ifThenElse :: Bool -> a -> a -> a
ifThenElse b x y = if b then x else y

setField :: forall x r a. GHC.Records.Compat.HasField x r a => r -> a -> r
setField = fst . GHC.Records.Compat.hasField @x
