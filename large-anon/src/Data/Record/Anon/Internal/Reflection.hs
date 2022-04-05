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
  , reflectProject
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

newtype WK r     = WK (KnownFields r     => Reflected (KnownFields r))
newtype WA r c   = WA (AllFields r c     => Reflected (AllFields r c))
newtype WP r r'  = WP (Project r r'      => Reflected (Project r r'))
newtype WR n r a = WR (RowHasField n r a => Reflected (RowHasField n r a))

reflectKnownFields :: DictKnownFields k r     -> Reflected (KnownFields r)
reflectAllFields   :: DictAllFields k r c     -> Reflected (AllFields r c)
reflectProject     :: DictProject k r r'      -> Reflected (Project r r')
reflectRowHasField :: DictRowHasField k n r a -> Reflected (RowHasField n r a)

reflectKnownFields f = noInlineUnsafeCo (WK Reflected) f
reflectAllFields   f = noInlineUnsafeCo (WA Reflected) f
reflectProject     f = noInlineUnsafeCo (WP Reflected) f
reflectRowHasField f = noInlineUnsafeCo (WR Reflected) f
