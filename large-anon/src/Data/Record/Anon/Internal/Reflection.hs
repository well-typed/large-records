{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}

-- | Low-level reflection utility for internal use.
--
-- Intended for qualified import:
--
-- > import Data.Record.Anon.Internal.Reflection (Reflected(..))
-- > import qualified Data.Record.Anon.Internal.Reflection as Unsafe
module Data.Record.Anon.Internal.Reflection (
    Reflected(..)
  , reflectKnownFields
  , reflectAllFields
  , reflectSubRow
  , reflectRowHasField
  ) where

import Data.Record.Anon.Plugin.Internal.Runtime

{-------------------------------------------------------------------------------
  Dictionary
-------------------------------------------------------------------------------}

-- | Evidence of some constraint @c@
--
-- This is like 'Data.Record.Anon.Dict', but without the functor argument.
data Reflected c where
  Reflected :: c => Reflected c

{-------------------------------------------------------------------------------
  Reflection
-------------------------------------------------------------------------------}

newtype WK r     = MkWK (KnownFields r     => Reflected (KnownFields r))
newtype WA r c   = MkWA (AllFields r c     => Reflected (AllFields r c))
newtype WS r r'  = MkWS (SubRow r r'       => Reflected (SubRow r r'))
newtype WR n r a = MkWR (RowHasField n r a => Reflected (RowHasField n r a))

reflectKnownFields :: DictKnownFields k r     -> Reflected (KnownFields r)
reflectAllFields   :: DictAllFields k r c     -> Reflected (AllFields r c)
reflectSubRow      :: DictSubRow k r r'       -> Reflected (SubRow r r')
reflectRowHasField :: DictRowHasField k n r a -> Reflected (RowHasField n r a)

reflectKnownFields f = noInlineUnsafeCo (MkWK Reflected) f
reflectAllFields   f = noInlineUnsafeCo (MkWA Reflected) f
reflectSubRow      f = noInlineUnsafeCo (MkWS Reflected) f
reflectRowHasField f = noInlineUnsafeCo (MkWR Reflected) f
