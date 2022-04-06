{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}

-- | Interop with standard GHC generics
module Data.Record.Generic.GHC (
    -- * From GHC to LR generics
    ThroughLRGenerics(..)
    -- * GHC generics metadata
  , GhcMetadata(..)
  , GhcFieldMetadata(..)
  , ghcMetadata
  ) where

import Data.Kind
import Data.Proxy
import GHC.Generics hiding (Generic(..), Rep)
import GHC.TypeLits

import Data.Record.Generic

import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  From GHC to LR generics
-------------------------------------------------------------------------------}

-- | Route from GHC generics to LR generics
--
-- Suppose a function such as
--
-- > allEqualTo :: Eq a => a -> [a] -> Bool
-- > allEqualTo x = all (== x)
--
-- is instead written as
--
-- > allEqualTo :: (GHC.Generic a, GHC.GEq' (GHC.Rep a)) => a -> [a] -> Bool
-- > allEqualTo x = all (GHC.geqdefault x)
--
-- where instead of using an indirection through an auxiliary type class `Eq`,
-- it directly assumes @GHC.Generics@ and uses a concrete generic
-- implementation. Such design is arguably questionable, but for example
-- @beam-core@ contains many such deeply ingrained assumptions of the
-- availability of @GHC.Generics@.
--
-- In order to be able to call such a function on a large record @Foo@,
-- 'largeRecord' will generate an instance
--
-- > instance GHC.Generic Foo where
-- >   type Rep Foo = ThroughLRGenerics Foo
-- >
-- >   from = WrapThroughLRGenerics
-- >   to   = unwrapThroughLRGenerics
--
-- For our running example, this instance makes it possible to call 'allEqualTo'
-- provided we then provide an instance
--
-- > instance ( LR.Generic a
-- >          , LR.Constraints a Eq
-- >          ) => GHC.GEq' (ThroughLRGenerics a) where
-- >   geq' = LR.geq `on` unwrapThroughLRGenerics
--
-- Effectively, 'ThroughLRGenerics' can be used to redirect a function that uses
-- GHC generics to a function that uses LR generics.
newtype ThroughLRGenerics a p = WrapThroughLRGenerics {
      unwrapThroughLRGenerics :: a
    }

{-------------------------------------------------------------------------------
  GHC generics metadata
-------------------------------------------------------------------------------}

-- | GHC generics metadata
--
-- TODO: Currently we provide metadata only for the record fields, not the
-- constructor or type name
data GhcMetadata a = GhcMetadata {
      ghcMetadataFields :: Rep GhcFieldMetadata a
    }

data GhcFieldMetadata :: Type -> Type where
  GhcFieldMetadata :: forall (f :: Meta) (a :: Type).
       Selector f
    => Proxy f -> GhcFieldMetadata a

withFieldMetadata :: forall (s :: Symbol) (r :: Type).
     KnownSymbol s
  => Proxy s
  -> SourceUnpackedness
  -> SourceStrictness
  -> DecidedStrictness
  -> (forall (f :: Meta). Selector f => Proxy f -> r)
  -> r
-- Uff, why the hell SignI is not exported from GHC.Generics???
withFieldMetadata _ NoSourceUnpackedness NoSourceStrictness DecidedLazy   k = k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy  ))
withFieldMetadata _ NoSourceUnpackedness NoSourceStrictness DecidedStrict k = k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedStrict))
withFieldMetadata _ NoSourceUnpackedness NoSourceStrictness DecidedUnpack k = k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedUnpack))
withFieldMetadata _ NoSourceUnpackedness SourceLazy         DecidedLazy   k = k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'SourceLazy         'DecidedLazy  ))
withFieldMetadata _ NoSourceUnpackedness SourceLazy         DecidedStrict k = k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'SourceLazy         'DecidedStrict))
withFieldMetadata _ NoSourceUnpackedness SourceLazy         DecidedUnpack k = k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'SourceLazy         'DecidedUnpack))
withFieldMetadata _ NoSourceUnpackedness SourceStrict       DecidedLazy   k = k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'SourceStrict       'DecidedLazy  ))
withFieldMetadata _ NoSourceUnpackedness SourceStrict       DecidedStrict k = k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'SourceStrict       'DecidedStrict))
withFieldMetadata _ NoSourceUnpackedness SourceStrict       DecidedUnpack k = k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'SourceStrict       'DecidedUnpack))
withFieldMetadata _ SourceNoUnpack       NoSourceStrictness DecidedLazy   k = k (Proxy @('MetaSel ('Just s) 'SourceNoUnpack       'NoSourceStrictness 'DecidedLazy  ))
withFieldMetadata _ SourceNoUnpack       NoSourceStrictness DecidedStrict k = k (Proxy @('MetaSel ('Just s) 'SourceNoUnpack       'NoSourceStrictness 'DecidedStrict))
withFieldMetadata _ SourceNoUnpack       NoSourceStrictness DecidedUnpack k = k (Proxy @('MetaSel ('Just s) 'SourceNoUnpack       'NoSourceStrictness 'DecidedUnpack))
withFieldMetadata _ SourceNoUnpack       SourceLazy         DecidedLazy   k = k (Proxy @('MetaSel ('Just s) 'SourceNoUnpack       'SourceLazy         'DecidedLazy  ))
withFieldMetadata _ SourceNoUnpack       SourceLazy         DecidedStrict k = k (Proxy @('MetaSel ('Just s) 'SourceNoUnpack       'SourceLazy         'DecidedStrict))
withFieldMetadata _ SourceNoUnpack       SourceLazy         DecidedUnpack k = k (Proxy @('MetaSel ('Just s) 'SourceNoUnpack       'SourceLazy         'DecidedUnpack))
withFieldMetadata _ SourceNoUnpack       SourceStrict       DecidedLazy   k = k (Proxy @('MetaSel ('Just s) 'SourceNoUnpack       'SourceStrict       'DecidedLazy  ))
withFieldMetadata _ SourceNoUnpack       SourceStrict       DecidedStrict k = k (Proxy @('MetaSel ('Just s) 'SourceNoUnpack       'SourceStrict       'DecidedStrict))
withFieldMetadata _ SourceNoUnpack       SourceStrict       DecidedUnpack k = k (Proxy @('MetaSel ('Just s) 'SourceNoUnpack       'SourceStrict       'DecidedUnpack))
withFieldMetadata _ SourceUnpack         NoSourceStrictness DecidedLazy   k = k (Proxy @('MetaSel ('Just s) 'SourceUnpack         'NoSourceStrictness 'DecidedLazy  ))
withFieldMetadata _ SourceUnpack         NoSourceStrictness DecidedStrict k = k (Proxy @('MetaSel ('Just s) 'SourceUnpack         'NoSourceStrictness 'DecidedStrict))
withFieldMetadata _ SourceUnpack         NoSourceStrictness DecidedUnpack k = k (Proxy @('MetaSel ('Just s) 'SourceUnpack         'NoSourceStrictness 'DecidedUnpack))
withFieldMetadata _ SourceUnpack         SourceLazy         DecidedLazy   k = k (Proxy @('MetaSel ('Just s) 'SourceUnpack         'SourceLazy         'DecidedLazy  ))
withFieldMetadata _ SourceUnpack         SourceLazy         DecidedStrict k = k (Proxy @('MetaSel ('Just s) 'SourceUnpack         'SourceLazy         'DecidedStrict))
withFieldMetadata _ SourceUnpack         SourceLazy         DecidedUnpack k = k (Proxy @('MetaSel ('Just s) 'SourceUnpack         'SourceLazy         'DecidedUnpack))
withFieldMetadata _ SourceUnpack         SourceStrict       DecidedLazy   k = k (Proxy @('MetaSel ('Just s) 'SourceUnpack         'SourceStrict       'DecidedLazy  ))
withFieldMetadata _ SourceUnpack         SourceStrict       DecidedStrict k = k (Proxy @('MetaSel ('Just s) 'SourceUnpack         'SourceStrict       'DecidedStrict))
withFieldMetadata _ SourceUnpack         SourceStrict       DecidedUnpack k = k (Proxy @('MetaSel ('Just s) 'SourceUnpack         'SourceStrict       'DecidedUnpack))

ghcMetadata :: Generic a => proxy a -> GhcMetadata a
ghcMetadata pa = GhcMetadata {
      ghcMetadataFields = Rep.map ghcFieldMetadata recordFieldMetadata
    }
  where
    Metadata{..} = metadata pa

    ghcFieldMetadata :: FieldMetadata x -> GhcFieldMetadata x
    ghcFieldMetadata (FieldMetadata pName su ss ds) =
        withFieldMetadata pName su ss ds $ GhcFieldMetadata
