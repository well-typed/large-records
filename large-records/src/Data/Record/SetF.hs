{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Record.SetF (
  setF,
  (&),
) where

import GHC.Records.Compat (HasField, setField)
import Data.Function ((&))

-- | Shorter name of 'setField', with order flipped to allow better composition.
--
-- The HasField Redesign proposal https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0583-hasfield-redesign.rst also changes the order of arguments.
setF :: forall k a r. HasField k r a => a -> r -> r
setF a r = setField @k r a
