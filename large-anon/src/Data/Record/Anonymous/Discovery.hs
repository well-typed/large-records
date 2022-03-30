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
    SomeField(..)
  , Shape(..)
  , discoverShape
    -- * Discover projections
  , discoverProjection
  , discoverLens
    -- * Constraints
  , discoverConstraint
  ) where

import Data.Kind
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Dict
import GHC.Exts (Any)

import Data.Record.Anon.Plugin.Internal.Runtime

import qualified Data.Record.Anon.Core.Canonical as Canon

import Data.Record.Anonymous.Advanced (lens)
import Data.Record.Anonymous.Internal.Record (Record)
import Data.Record.Anonymous.Internal.Reflection
import Data.Record.Anonymous.Internal.Row.KnownRow (CannotProject, KnownRow)

import qualified Data.Record.Anonymous.Internal.Record         as Record
import qualified Data.Record.Anonymous.Internal.Row.KnownField as KnownField
import qualified Data.Record.Anonymous.Internal.Row.KnownRow   as KnownRow

{-------------------------------------------------------------------------------
  Shape
-------------------------------------------------------------------------------}

data SomeField (f :: k -> Type) where
  SomeField :: forall k (f :: k -> Type) (x :: k). f x -> SomeField f

data Shape (f :: k -> Type) where
  Shape :: forall k (f :: k -> Type) (r :: Row k).
       KnownFields r
    => Record f r -> Shape f

-- | Discover record shape
--
-- Shape discovery is the first step when dealing with records of an unknown
-- row: pattern matching on the resulting 'Shape' brings into scope an
-- existentially quantified row @r@, along with a record over @r@ containing
-- the specified fields.
--
-- Once the shape has been discovered, there are two alternative next steps:
--
-- 1. Use 'discoverConstraint' to discover additional 'AllFields' constraints
--    for @r@. To discover @AllFields r c@, you will need to translate every
--    field of some type @f x@ to @Dict c x@. This will require a careful choice
--    of @f@. For example, if we choose @f = IsValue@
--
--    > data IsValue x where
--    >   IsValue :: Show x => x -> IsValue x
--
--    then @IsValue x@ will allow us to construct @Dict Show x@.
--
--    This option is primarily useful when dealing with records only
--    "wholesale", never accessing (statically known) fields of the record.
--
-- 2. Alternatively, use 'discoverProjection' to check whether there is a lens
--    from the unknown record to a record of another type. Typically that other
--    type then /is/ statically known, which means that once we have such a
--    lens, further discovery of additional constraints is not necesssary:
--    these constraints can be proved in the normal way for the concrete type.
discoverShape :: forall k (f :: k -> Type). [(String, SomeField f)] -> Shape f
discoverShape fields =
    mkSomeRecord $
      reflectKnownFields (\_proxyR -> map fst fields)
  where
    mkSomeRecord :: forall (r :: Row k).
         Reflected (KnownFields r)
      -> Shape f
    mkSomeRecord Reflected = Shape @k @f @r . Record.unsafeFromCanonical $
        Canon.fromList $ map (\(_, SomeField fx) -> co fx) fields

    co :: f x -> f Any
    co = noInlineUnsafeCo

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

-- | Discover additional constraints for an unknown record
--
-- See 'discoverShape' for a detailed discussion.
discoverConstraint :: forall k (c :: k -> Constraint) (r :: Row k).
     Record (Dict c) r
  -> Reflected (AllFields r c)
discoverConstraint dicts =
    reflectAllFields $ \_ _ ->
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
-- See 'discoverShape' for additional discussion.
discoverProjection :: forall k (r :: Row k) (r' :: Row k) a proxy proxy'.
     (KnownFields r, KnownFields r')
  => proxy  r
  -> proxy' r'
  -> Either CannotProject (Reflected (Project (K a) r r'))
discoverProjection _ _ =
    go . map fst <$>
      KnownRow.canProject
        (mkKnownRow $ fieldNames (Proxy @r))
        (mkKnownRow $ fieldNames (Proxy @r'))
  where
    mkKnownRow :: [String] -> KnownRow ()
    mkKnownRow = KnownRow.fromList . map KnownField.fromString

    go :: [Int] -> Reflected (Project (K a) r r')
    go proj = reflectProject $ \_ _ _ -> proj

discoverLens :: forall k (r :: Row k) (r' :: Row k) (a :: Type) proxy proxy'.
     (KnownFields r, KnownFields r')
  => proxy  r
  -> proxy' r'
  -> Either CannotProject
            (Record (K a) r -> ( Record (K a) r'
                               , Record (K a) r' -> Record (K a) r
                               ))
discoverLens pr pr' = aux <$> discoverProjection pr pr'
  where
    aux :: Reflected (Project (K a) r r')
        -> Record (K a) r
        -> (Record (K a) r', Record (K a) r' -> Record (K a) r)
    aux Reflected = lens

