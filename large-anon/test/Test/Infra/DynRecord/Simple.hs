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

import Data.Record.Anon
import Data.Record.Anon.Advanced (CannotProject)
import Data.Record.Anon.Simple (Record)

import qualified Data.Record.Anon.Advanced as A
import qualified Data.Record.Anon.Simple   as S

import Test.Infra.DynRecord

import qualified Test.Infra.DynRecord.Advanced as A.Dyn

{-------------------------------------------------------------------------------
  Projection to known row
-------------------------------------------------------------------------------}

toLens ::
     ( KnownFields r
     , AllFields   r (FromValue I)
     , AllFields   r (ToValue   I)
     )
  => proxy r
  -> DynRecord
  -> Either (Either CannotProject ParseError)
            (Record r, Record r -> DynRecord)
toLens p = fmap (bimap S.fromAdvanced (. S.toAdvanced)) . A.Dyn.toLens p

toRecord ::
     (KnownFields r, AllFields r (FromValue I))
  => proxy r
  -> DynRecord
  -> Either (Either CannotProject ParseError) (Record r)
toRecord p = fmap S.fromAdvanced . A.Dyn.toRecord p

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
       ( Show        x
       , Eq          x
       , ToValue   I x
       , FromValue I x
       )
    => String -> x -> ValidField x

data SomeRecord where
  SomeRecord :: forall (r :: Row Type).
       ( KnownFields r
       , AllFields r Show
       , AllFields r Eq
       , AllFields r (FromValue I)
       , AllFields r (ToValue   I)
       )
    => Record r -> SomeRecord

deriving instance Show SomeRecord

inferType :: DynRecord -> SomeRecord
inferType (DynRecord r) =
    case A.someRecord $ map (uncurry mkField) r of
      Some record ->
        case A.reflectKnownFields $ A.map fieldName record of
          Reflected -> withSomeRecord record
  where
    withSomeRecord :: KnownFields r => A.Record ValidField r -> SomeRecord
    withSomeRecord record =
        case ( A.reflectAllFields (A.map dictShow      record)
             , A.reflectAllFields (A.map dictEq        record)
             , A.reflectAllFields (A.map dictFromValue record)
             , A.reflectAllFields (A.map dictToValue   record)
             ) of
          (Reflected, Reflected, Reflected, Reflected) ->
            SomeRecord (S.fromAdvanced $ A.map fieldValue record)

    fieldName  :: ValidField x -> K String x
    fieldValue :: ValidField x -> I x

    fieldName  (ValidField name _    ) = K name
    fieldValue (ValidField _    value) = I value

    dictShow      :: ValidField x -> Dict Show x
    dictEq        :: ValidField x -> Dict Eq   x
    dictFromValue :: ValidField x -> Dict (FromValue I) x
    dictToValue   :: ValidField x -> Dict (ToValue   I) x

    dictShow      (ValidField _ _) = Dict
    dictEq        (ValidField _ _) = Dict
    dictFromValue (ValidField _ _) = Dict
    dictToValue   (ValidField _ _) = Dict

    mkField :: String -> Value -> Some ValidField
    mkField name (VI x) = Some $ ValidField name x
    mkField name (VB x) = Some $ ValidField name x
    mkField name (VC x) = Some $ ValidField name x
