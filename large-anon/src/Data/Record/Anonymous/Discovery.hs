{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
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
  , someRecord
    -- * Discover projections
  , reflectProject
    -- * Constraints
  , reflectAllFields
  , reflectKnownFields
  ) where

import Data.Kind
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Dict
import GHC.Exts (Any)

import Data.Record.Anon.Plugin.Internal.Runtime

import qualified Data.Record.Anon.Core.Canonical as Canon

import Data.Record.Anonymous.Advanced (collapse)
import Data.Record.Anonymous.Internal.Record (Record)
import Data.Record.Anonymous.Internal.Reflection
import Data.Record.Anonymous.Internal.Row.KnownRow (CannotProject, KnownRow)

import qualified Data.Record.Anonymous.Internal.Record         as Record
import qualified Data.Record.Anonymous.Internal.Row.KnownField as KnownField
import qualified Data.Record.Anonymous.Internal.Row.KnownRow   as KnownRow

{-------------------------------------------------------------------------------
  Shape
-------------------------------------------------------------------------------}

-- | Existential type ("there exists an @x@ such that @f x@")
data Some (f :: k -> Type) where
  Some :: forall k (f :: k -> Type) (x :: k). f x -> Some f

someRecord :: forall k (f :: k -> Type). [Some f] -> Some (Record f)
someRecord fields =
   Some $ Record.unsafeFromCanonical $
      Canon.fromList $ map (\(Some x) -> co x) fields
  where
    co :: f x -> f Any
    co = noInlineUnsafeCo

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

reflectKnownFields :: forall k (r :: Row k).
     Record (K String) r
  -> Reflected (KnownFields r)
reflectKnownFields names =
    unsafeReflectKnownFields $ \_ -> collapse names

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
-------------------------------------------------------------------------------}

-- | Runtime check if we can project from one record to another
--
-- Since we cannot do runtime type checks, we insist that the fields of the
-- record must all be of one type @a@.
--
-- See 'discoverRow' for additional discussion.
reflectProject :: forall k (r :: Row k) (r' :: Row k) a proxy proxy'.
     (KnownFields r, KnownFields r')
  => proxy  r
  -> proxy' r'
  -> Either CannotProject (Reflected (Project (K a) r r'))
reflectProject _ _ =
    go . map fst <$>
      KnownRow.canProject
        (mkKnownRow $ fieldNames (Proxy @r))
        (mkKnownRow $ fieldNames (Proxy @r'))
  where
    mkKnownRow :: [String] -> KnownRow ()
    mkKnownRow = KnownRow.fromList . map KnownField.fromString

    go :: [Int] -> Reflected (Project (K a) r r')
    go proj = unsafeReflectProject $ \_ _ _ -> proj

