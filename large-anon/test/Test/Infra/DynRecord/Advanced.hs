{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

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
  , IsValue(..)
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
    case discoverShape $ map (second (SomeField . K)) r of
      Shape s ->
        (fmap setter  . ($ s)) <$> discoverLens s p
  where
    -- @r'@ is the "inferred" row of the actual 'DynRecord'
    -- (as opposed to the expected row specified as an argument)
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

data IsValue (f :: k -> Type) (x :: k) where
  IsValue ::
       ( Show     (f x)
       , Eq       (f x)
       , ToValue   f x
       , FromValue f x
       )
    => f x -> IsValue f x

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
     (Value -> SomeField (IsValue f))
  -> DynRecord
  -> SomeRecord f
inferType mkField (DynRecord r) =
    case discoverShape (map (second mkField) r) of
      Shape s ->
        case ( discoverConstraint (Anon.map dictShow      s)
             , discoverConstraint (Anon.map dictEq        s)
             , discoverConstraint (Anon.map dictFromValue s)
             , discoverConstraint (Anon.map dictToValue   s)
             ) of
          (Reflected, Reflected, Reflected, Reflected) ->
            SomeRecord (aux s)
  where
    aux :: Record (IsValue f) r -> Record f r
    aux = Anon.map (\(IsValue x) -> x)

    dictShow      :: IsValue f x -> Dict (Compose Show f) x
    dictEq        :: IsValue f x -> Dict (Compose Eq   f) x
    dictFromValue :: IsValue f x -> Dict (FromValue    f) x
    dictToValue   :: IsValue f x -> Dict (ToValue      f) x

    dictShow      (IsValue _) = Dict
    dictEq        (IsValue _) = Dict
    dictFromValue (IsValue _) = Dict
    dictToValue   (IsValue _) = Dict
