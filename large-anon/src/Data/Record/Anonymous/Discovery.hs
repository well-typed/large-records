{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Tools for type discovery for existentially quantified rows
--
-- This is useful at untyped/typed boundaries, for example when parsing JSON
-- values as records.
module Data.Record.Anonymous.Discovery (
    -- * Discover shape
    Some(..)
  , SomeRecord(..)
  , someRecord
    -- * KnownFields
  , reifyKnownFields
  , reflectKnownFields
    -- * AllFields
  , reflectAllFields
    -- * Project
  , InRow(..)
  , reifyProject
  , reflectProject
  ) where

import Data.Kind
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Dict
import GHC.Exts (Any)
import GHC.TypeLits

import Data.Record.Anon.Plugin.Internal.Runtime

import qualified Data.Record.Anon.Internal.Core.Canonical as Canon

import Data.Record.Anonymous.Advanced (collapse)
import Data.Record.Anonymous.Internal.Record (Record)
import Data.Record.Anonymous.Internal.Reflection

import qualified Data.Record.Anonymous.Internal.Record             as Record
import qualified Data.Record.Anonymous.Internal.Combinators.Simple as Simple
import Data.Functor.Product
import Data.Bifunctor

{-------------------------------------------------------------------------------
  KnownFields
-------------------------------------------------------------------------------}

reifyKnownFields :: forall k (r :: Row k) proxy.
     KnownFields r
  => proxy r -> Record (K String) r
reifyKnownFields _ =
    Record.unsafeFromCanonical $
      Canon.fromList $ map K $ fieldNames (Proxy @r)

reflectKnownFields :: forall k (r :: Row k).
     Record (K String) r
  -> Reflected (KnownFields r)
reflectKnownFields names =
    unsafeReflectKnownFields $ \_ -> collapse names

{-------------------------------------------------------------------------------
  AllFields
-------------------------------------------------------------------------------}

-- | Discover additional constraints for an unknown record
--
-- See 'discoverRow' for a detailed discussion.
reflectAllFields :: forall k (c :: k -> Constraint) (r :: Row k).
     Record (Dict c) r
  -> Reflected (AllFields r c)
reflectAllFields dicts =
    unsafeReflectAllFields $ \_ _ ->
      fmap aux $ Canon.toLazyVector $ Record.toCanonical dicts
  where
    aux :: Dict c Any -> DictAny c
    aux Dict = DictAny

{-------------------------------------------------------------------------------
  Projections

  The @KnownFields@ constraint on @reifyProject@ is a little dissatisfying, as
  it feels like an orthogonal concern. Ultimately the reason is that in

  > Record f (r :: Row k)

  we have @f :: k -> Type@, as opposed to @f :: Symbol -> k -> Type@. That is
  a generalization we could at some point consider, but until we do, the

  > RowHasField n r a

  constraint introduced in the body 'InRow' involves an /existential/ @n@;
  a /separate/ record with 'KnownSymbol' evidence would therefore not give us
  any information about /this/ @n@.
-------------------------------------------------------------------------------}

-- | @InRow r a@ is evidence that there exists some @n@ s.t. @(n := a)@ in @r@.
data InRow (r :: Row k) (a :: k) where
  InRow :: forall k (n :: Symbol) (r :: Row k) (a :: k).
       ( KnownSymbol n
       , RowHasField n r a
       )
    => Proxy n -> InRow r a

reifyProject :: forall k (r :: Row k) (r' :: Row k).
     (Project r r', KnownFields r')
  => Record (InRow r) r'
reifyProject =
    Simple.zipWith aux ixs (reifyKnownFields (Proxy @r'))
  where
    ixs :: Record (K Int) r'
    ixs = Record.unsafeFromCanonical $
            Canon.fromList $ map K $ projectIndices (Proxy @r) (Proxy @r')

    aux :: forall x. K Int x -> K String x -> InRow r x
    aux (K i) (K name) =
        case someSymbolVal name of
          SomeSymbol p -> unsafeInRow i p

reflectProject :: forall k (r :: Row k) (r' :: Row k).
     Record (InRow r) r'
  -> Reflected (Project r r')
reflectProject ixs =
    unsafeReflectProject $ \_ _ ->
      Simple.collapse $ Simple.map aux ixs
  where
    aux :: forall x. InRow r x -> K Int x
    aux (InRow p) = K $ rowHasField p (Proxy @r) (Proxy @x)

{-------------------------------------------------------------------------------
  Existential records
-------------------------------------------------------------------------------}

-- | Existential type ("there exists an @x@ such that @f x@")
data Some (f :: k -> Type) where
  Some :: forall k (f :: k -> Type) (x :: k). f x -> Some f

-- | Discovered row variable
--
-- See 'Data.Record.Anon.Advanced.someRecord' for detailed discussion.
data SomeRecord (f :: k -> Type) where
  SomeRecord :: forall k (r :: Row k) (f :: k -> Type).
       KnownFields r
    => Record (Product (InRow r) f) r
    -> SomeRecord f

someRecord :: forall k (f :: k -> Type). [(String, Some f)] -> SomeRecord f
someRecord fields =
    mkSomeRecord $
      Record.unsafeFromCanonical $
        Canon.fromList $ zipWith aux [0..] (map (first someSymbolVal) fields)
  where
    aux :: Int -> (SomeSymbol, Some f) -> Product (InRow r) f Any
    aux i (SomeSymbol n, Some fx) = Pair (unsafeInRow i n) (co fx)

    co :: f x -> f Any
    co = noInlineUnsafeCo

    mkSomeRecord :: forall r. Record (Product (InRow r) f) r -> SomeRecord f
    mkSomeRecord r =
        case reflected of
          Reflected -> SomeRecord r
      where
        reflected :: Reflected (KnownFields r)
        reflected = reflectKnownFields $ Simple.map getName r

        getName :: Product (InRow r) f x -> K String x
        getName (Pair (InRow p) _) = K $ symbolVal p

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

unsafeInRow :: forall n r a. KnownSymbol n => Int -> Proxy n -> InRow r a
unsafeInRow i p =
    case reflected of
      Reflected -> InRow p
  where
    reflected :: Reflected (RowHasField n r a)
    reflected = unsafeReflectRowHasField $ \_ _ _ -> i

