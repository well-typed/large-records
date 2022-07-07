{-# LANGUAGE ConstraintKinds     #-}
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
    -- * Type inference
    ValidField(..)
  , IsValue(..)
  , SomeRecord(..)
  , inferType
    -- * Lens
  , toLens
  , toRecord
  ) where

import Data.Bifunctor
import Data.Kind
import Data.Record.Generic
import Data.SOP.Constraint
import Data.Typeable

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

import Test.Infra.Discovery
import Test.Infra.DynRecord

{-------------------------------------------------------------------------------
  Type inference
-------------------------------------------------------------------------------}

data ValidField (f :: k -> Type) (x :: k) where
  ValidField ::
       ( Typeable    x
       , Show     (f x)
       , Eq       (f x)
       , ToValue   f x
       )
    => f x -> ValidField f x

class IsValue f where
  isValue :: Value -> Some (ValidField f)

data SomeRecord (f :: k -> Type) where
  SomeRecord :: forall k (f :: k -> Type) (r :: Row k).
       ( KnownFields r
       , SubRow r r
       , AllFields r Typeable
       , AllFields r (Compose Show f)
       , AllFields r (Compose Eq   f)
       , AllFields r (ToValue      f)
       )
    => Record f r -> SomeRecord f

inferType :: forall k (f :: k -> Type). IsValue f => DynRecord -> SomeRecord f
inferType (DynRecord r) =
     case Anon.someRecord (map (second isValue) r) of
       Anon.SomeRecord record ->
         case Anon.reflectSubRow (Anon.map pairFst record) of
           Reflected -> withSomeRecord (Anon.map pairSnd record)
  where
    withSomeRecord ::
         ( KnownFields r
         , SubRow r r
         )
      => Record (ValidField f) r -> SomeRecord f
    withSomeRecord record =
        case ( Anon.reflectAllFields (Anon.map dictTypeable  record)
             , Anon.reflectAllFields (Anon.map dictShow      record)
             , Anon.reflectAllFields (Anon.map dictEq        record)
             , Anon.reflectAllFields (Anon.map dictToValue   record)
             ) of
          (Reflected, Reflected, Reflected, Reflected) ->
            SomeRecord (Anon.map fieldValue record)

    fieldValue :: ValidField f x -> f x
    fieldValue (ValidField value) = value

    dictTypeable :: ValidField f x -> Dict Typeable         x
    dictShow     :: ValidField f x -> Dict (Compose Show f) x
    dictEq       :: ValidField f x -> Dict (Compose Eq   f) x
    dictToValue  :: ValidField f x -> Dict (ToValue      f) x

    dictTypeable (ValidField _) = Dict
    dictShow     (ValidField _) = Dict
    dictEq       (ValidField _) = Dict
    dictToValue  (ValidField _) = Dict

{-------------------------------------------------------------------------------
  Projection to known row
-------------------------------------------------------------------------------}

-- | Lens to record over some known row @r@
toLens :: forall k (f :: k -> Type) (r :: Row k) proxy.
     ( IsValue f
     , KnownFields r
     , SubRow r r
     , AllFields r Typeable
     )
  => proxy r
  -> DynRecord
  -> Either NotSubRow (Record f r, Record f r -> DynRecord)
toLens p = \r ->
    -- In order to be able to check if we can project to the known row @r@,
    -- we must first to type inference on the @DynRecord@. /If/ this succeeds,
    -- we know the types line up, and there can be no further type errors
    -- (there is no need for a separate parsing step).
    case inferType r of
      SomeRecord r' ->
        fmap (withSomeRecord r') $ checkIsSubRow r' p
  where
    -- @r'@ is the row inferred for the 'DynRecord'
    withSomeRecord :: forall (r' :: Row k).
         ( KnownFields r'
         , AllFields r' (ToValue f)
         )
      => Record f r'
      -> Reflected (SubRow r' r)
      -> (Record f r, Record f r -> DynRecord)
    withSomeRecord r Reflected = (
          getter
        , DynRecord . Anon.toList . toValues . setter
        )
      where
        getter :: Record f r
        setter :: Record f r -> Record f r'
        (getter, setter) = Anon.lens r

toRecord :: forall k (r :: Row k) (f :: k -> Type) proxy.
     ( IsValue f
     , KnownFields r
     , SubRow r r
     , AllFields r Typeable
     )
  => proxy r
  -> DynRecord
  -> Either NotSubRow (Record f r)
toRecord p = fmap fst . toLens p
