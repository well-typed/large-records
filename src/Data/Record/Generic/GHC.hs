{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

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
  -> FieldStrictness
  -> (forall (f :: Meta). Selector f => Proxy f -> r)
  -> r
withFieldMetadata _ s k =
    case s of
      FieldLazy   -> k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy))
      FieldStrict -> k (Proxy @('MetaSel ('Just s) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedStrict))

ghcMetadata :: Generic a => proxy a -> GhcMetadata a
ghcMetadata pa = GhcMetadata {
      ghcMetadataFields = Rep.map ghcFieldMetadata recordFieldInfo
    }
  where
    Metadata{..} = metadata pa

    ghcFieldMetadata :: FieldInfo x -> GhcFieldMetadata x
    ghcFieldMetadata (FieldInfo pName s) = withFieldMetadata pName s $ \p' ->
        GhcFieldMetadata p'
