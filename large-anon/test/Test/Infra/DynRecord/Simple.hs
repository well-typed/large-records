{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- | 'DynRecord' interop with the simple record API.
--
-- Intended for qualified import.
--
-- > import qualified Test.Infra.DynRecord.Simple as Dyn
module Test.Infra.DynRecord.Simple (
    -- * Type inference
    ValidField(..)
  , SomeRecord(..)
  , inferType
    -- * Lens
  , toLens
  , toRecord
  ) where

import Data.Bifunctor
import Data.Kind
import Data.Typeable

import Data.Record.Anon
import Data.Record.Anon.Simple (Record)

import qualified Data.Record.Anon.Advanced as A
import qualified Data.Record.Anon.Simple   as S

import Test.Infra.Discovery
import Test.Infra.DynRecord

{-------------------------------------------------------------------------------
  Type inference

  NOTE: This canont be defined in terms of A.Dyn.inferType, because if we did,
  we would get in scope @AllFields r (Compose Show I)@, from which we cannot
  deduce @AllFields r Show@: @Show (I x)@ does not imply @Show x@. We could
  potentially solve this using a Beam-style HKD definition, but it's not that
  relevant for this test case.
-------------------------------------------------------------------------------}

data ValidField x where
  ValidField ::
       ( Typeable  x
       , Show      x
       , Eq        x
       , ToValue I x
       )
    => x -> ValidField x

data SomeRecord where
  SomeRecord :: forall (r :: Row Type).
       ( KnownFields r
       , Project r r
       , AllFields r Typeable
       , AllFields r Show
       , AllFields r Eq
       , AllFields r (ToValue I)
       )
    => Record r -> SomeRecord

inferType :: DynRecord -> SomeRecord
inferType (DynRecord r) =
    case A.someRecord $ map (second mkField) r of
      A.SomeRecord record ->
        case A.reflectProject (A.map pairFst record) of
          Reflected -> withSomeRecord (A.map pairSnd record)
  where
    withSomeRecord ::
         ( KnownFields r
         , Project r r
         )
      => A.Record ValidField r -> SomeRecord
    withSomeRecord record =
        case ( A.reflectAllFields (A.map dictTypeable  record)
             , A.reflectAllFields (A.map dictShow      record)
             , A.reflectAllFields (A.map dictEq        record)
             , A.reflectAllFields (A.map dictToValue   record)
             ) of
          (Reflected, Reflected, Reflected, Reflected) ->
            SomeRecord (S.fromAdvanced $ A.map fieldValue record)

    fieldValue :: ValidField x -> I x
    fieldValue (ValidField value) = I value

    dictTypeable  :: ValidField x -> Dict Typeable    x
    dictShow      :: ValidField x -> Dict Show        x
    dictEq        :: ValidField x -> Dict Eq          x
    dictToValue   :: ValidField x -> Dict (ToValue I) x

    dictTypeable (ValidField _) = Dict
    dictShow     (ValidField _) = Dict
    dictEq       (ValidField _) = Dict
    dictToValue  (ValidField _) = Dict

    mkField :: Value -> Some ValidField
    mkField (VI x) = Some $ ValidField x
    mkField (VB x) = Some $ ValidField x
    mkField (VC x) = Some $ ValidField x

{-------------------------------------------------------------------------------
  Projection to known row
-------------------------------------------------------------------------------}

-- | Lens to record over some known row @r@
toLens :: forall (r :: Row Type) proxy.
     ( KnownFields r
     , Project r r
     , AllFields r Typeable
     )
  => proxy r
  -> DynRecord
  -> Either CannotProject (Record r, Record r -> DynRecord)
toLens p = \r ->
    -- In order to be able to check if we can project to the known row @r@,
    -- we must first to type inference on the @DynRecord@. /If/ this succeeds,
    -- we know the types line up, and there can be no further type errors
    -- (there is no need for a separate parsing step).
    case inferType r of
      SomeRecord r' ->
        fmap (withSomeRecord r') $ checkCanProject r' p
  where
    -- @r'@ is the row inferred for the 'DynRecord'
    withSomeRecord :: forall (r' :: Row Type).
         ( KnownFields r'
         , AllFields r' (ToValue I)
         )
      => Record r'
      -> Reflected (Project r' r)
      -> (Record r, Record r -> DynRecord)
    withSomeRecord r Reflected = (
          getter
        , DynRecord . A.toList . toValues . S.toAdvanced . setter
        )
      where
        getter :: Record r
        setter :: Record r -> Record r'
        (getter, setter) = S.lens r

toRecord :: forall (r :: Row Type) proxy.
     ( KnownFields r
     , Project r r
     , AllFields r Typeable
     )
  => proxy r
  -> DynRecord
  -> Either CannotProject (Record r)
toRecord p = fmap fst . toLens p
