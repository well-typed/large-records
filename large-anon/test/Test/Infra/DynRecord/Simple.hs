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
  , IsValue(..)
  , SomeRecord(..)
  , inferType
  ) where

import Data.Bifunctor
import Data.Kind
import Data.SOP.BasicFunctors

import Data.Record.Anonymous.Discovery
import Data.Record.Anonymous.Internal.Reflection
import Data.Record.Anonymous.Internal.Row.KnownRow (CannotProject)
import Data.Record.Anonymous.Simple (Record, AllFields, KnownFields, Row)

import qualified Data.Record.Anonymous.Simple   as S
import qualified Data.Record.Anonymous.Advanced as A

import Test.Infra.DynRecord

import qualified Test.Infra.DynRecord.Advanced as A.Dyn
import Data.SOP.Dict

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

data IsValue x where
  IsValue ::
       ( Show        x
       , Eq          x
       , ToValue   I x
       , FromValue I x
       )
    => x -> IsValue x

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
    case discoverShape (map (second mkField) r) of
      Shape s ->
        case ( discoverConstraint (A.map dictShow      s)
             , discoverConstraint (A.map dictEq        s)
             , discoverConstraint (A.map dictFromValue s)
             , discoverConstraint (A.map dictToValue   s)
             ) of
          (Reflected, Reflected, Reflected, Reflected) ->
            SomeRecord (aux s)
  where
    mkField :: Value -> SomeField IsValue
    mkField (VI x) = SomeField (IsValue x)
    mkField (VB x) = SomeField (IsValue x)
    mkField (VC x) = SomeField (IsValue x)

    aux :: A.Record IsValue r -> Record r
    aux = S.fromAdvanced . A.map (\(IsValue x) -> I x)

    dictShow      :: IsValue x -> Dict Show x
    dictEq        :: IsValue x -> Dict Eq   x
    dictFromValue :: IsValue x -> Dict (FromValue I) x
    dictToValue   :: IsValue x -> Dict (ToValue   I) x

    dictShow      (IsValue _) = Dict
    dictEq        (IsValue _) = Dict
    dictFromValue (IsValue _) = Dict
    dictToValue   (IsValue _) = Dict

