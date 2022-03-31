{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

-- | 'DynRecord' interop with the advanced record API.
--
-- Intended for qualified import
--
-- > import qualified Test.Infra.DynRecord.Advanced as Dyn
module Test.Infra.DynRecord.Advanced (
    -- * Projection to known row
    toLens
  , toRecord
    -- * Type inference
  , ValidField(..)
  , SomeRecord(..)
  , inferType
  ) where

import Data.Bifunctor
import Data.Kind
import Data.Record.Generic
import Data.SOP.Constraint

import Data.Record.Anonymous.Advanced (Record, KnownFields, Row, AllFields)
import Data.Record.Anonymous.Discovery
import Data.Record.Anonymous.Internal.Reflection
import Data.Record.Anonymous.Internal.Row.KnownRow (CannotProject)

import qualified Data.Record.Anonymous.Advanced as Anon

import Test.Infra.DynRecord

{-------------------------------------------------------------------------------
  Projection to known row
-------------------------------------------------------------------------------}

-- | Internal auxiliary
--
-- Unlike 'toLens', this does not do any parsing or unparsing. This means that
-- we avoid type class constraints here, avoiding unnecessary constraints in
-- 'toRecord'.
toLens' :: forall k (r :: Row k) proxy.
     KnownFields r
  => proxy r
  -> DynRecord
  -> Either CannotProject
            (Record (K Value) r, Record (K Value) r -> DynRecord)
toLens' p (DynRecord r) =
    case discoverRow $ map (Some . K) r of
      Some record ->
        case discoverKnownFields (Anon.map (mapKK fst) record) of
          Reflected -> withSomeRecord (Anon.map (mapKK snd) record)
  where
    -- @r'@ is the "inferred" row of the actual 'DynRecord'
    -- (as opposed to the expected row specified as an argument)
    withSomeRecord :: forall (r' :: Row k).
          KnownFields r'
       => Record (K Value) r'
       -> Either CannotProject
                 (Record (K Value) r, Record (K Value) r -> DynRecord)
    withSomeRecord record =
        (fmap setter  . ($ record)) <$> discoverLens record p

    setter ::
         KnownFields r'
      => (Record (K Value) r -> Record (K Value) r')
      -> Record (K Value) r -> DynRecord
    setter f = DynRecord . Anon.toList . f

-- | Lens to record of known type
--
-- If the expected type of the record is known a priori, there is no need for a
-- separate type discovery step: we just check if the 'DynRecord' can be made
-- fit into the expected mould.
toLens ::
     ( KnownFields r
     , AllFields   r (FromValue f)
     , AllFields   r (ToValue   f)
     )
  => proxy r
  -> DynRecord
  -> Either (Either CannotProject ParseError)
            (Record f r, Record f r -> DynRecord)
toLens p = distrib . fmap (bimap fromValues (. toValues)) . toLens' p
  where
    distrib :: Either a (Either b c, d) -> Either (Either a b) (c, d)
    distrib (Left a)             = Left (Left a)
    distrib (Right (Left b, _))  = Left (Right b)
    distrib (Right (Right c, d)) = Right (c, d)

toRecord ::
     (KnownFields r, AllFields r (FromValue f))
  => proxy r
  -> DynRecord
  -> Either (Either CannotProject ParseError) (Record f r)
toRecord p = distrib . fmap (fromValues . fst) . toLens' p
  where
    distrib :: Either a (Either b c) -> Either (Either a b) c
    distrib (Left a)          = Left (Left a)
    distrib (Right (Left b))  = Left (Right b)
    distrib (Right (Right c)) = Right c

{-------------------------------------------------------------------------------
  Type inference
-------------------------------------------------------------------------------}

data ValidField (f :: k -> Type) (x :: k) where
  ValidField ::
       ( Show     (f x)
       , Eq       (f x)
       , ToValue   f x
       , FromValue f x
       )
    => String -> f x -> ValidField f x

data SomeRecord (f :: k -> Type) where
  SomeRecord :: forall k (f :: k -> Type) (r :: Row k).
       ( KnownFields r
       , AllFields r (Compose Show f)
       , AllFields r (Compose Eq   f)
       , AllFields r (FromValue    f)
       , AllFields r (ToValue      f)
       )
    => Record f r -> SomeRecord f

deriving instance Show (SomeRecord f)

inferType :: forall k (f :: k -> Type).
     (String -> Value -> Some (ValidField f))
  -> DynRecord
  -> SomeRecord f
inferType mkField (DynRecord r) =
    case discoverRow $ map (uncurry mkField) r of
      Some record ->
        case discoverKnownFields $ Anon.map fieldName record of
          Reflected -> withSomeRecord record
  where
    withSomeRecord :: KnownFields r => Record (ValidField f) r -> SomeRecord f
    withSomeRecord record =
        case ( discoverConstraint (Anon.map dictShow      record)
             , discoverConstraint (Anon.map dictEq        record)
             , discoverConstraint (Anon.map dictFromValue record)
             , discoverConstraint (Anon.map dictToValue   record)
             ) of
          (Reflected, Reflected, Reflected, Reflected) ->
            SomeRecord (Anon.map fieldValue record)

    fieldName  :: ValidField f x -> K String x
    fieldValue :: ValidField f x -> f x

    fieldName  (ValidField name _    ) = K name
    fieldValue (ValidField _    value) = value

    dictShow      :: ValidField f x -> Dict (Compose Show f) x
    dictEq        :: ValidField f x -> Dict (Compose Eq   f) x
    dictFromValue :: ValidField f x -> Dict (FromValue    f) x
    dictToValue   :: ValidField f x -> Dict (ToValue      f) x

    dictShow      (ValidField _ _) = Dict
    dictEq        (ValidField _ _) = Dict
    dictFromValue (ValidField _ _) = Dict
    dictToValue   (ValidField _ _) = Dict
