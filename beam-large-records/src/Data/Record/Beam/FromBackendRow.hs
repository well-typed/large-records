{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Record.Beam.FromBackendRow (
    GFromLargeBackendRow
  , FromBackendRowI
  ) where

import Data.Functor.Identity
import Data.List (foldl')
import Data.Proxy
import Data.Record.Generic
import Data.Record.Generic.GHC
import Data.Record.Generic.Transform
import Database.Beam.Backend.SQL.Row
import Database.Beam.Schema.Tables
import GHC.Generics hiding (Generic(..), (:.:))

import qualified Data.Record.Generic.Rep as Rep

import Data.Record.Beam.Interpretation

type GFromLargeBackendRow be tbl = (
    Generic (tbl Identity)
  , Generic (tbl Uninterpreted)
  , HasNormalForm (BeamInterpretation Identity) (tbl Identity) (tbl Uninterpreted)
  , Constraints (tbl Uninterpreted) (FromBackendRowI be)
  )

instance GFromLargeBackendRow be tbl
      => GFromBackendRow be (ThroughLRGenerics (tbl Exposed))
                            (ThroughLRGenerics (tbl Identity)) where
  gFromBackendRow _ =
      fmap (WrapThroughLRGenerics . to . denormalize1 (Proxy @BeamInterpretation)) $
        Rep.sequenceA perField
    where
      perField :: Rep (FromBackendRowM be :.: Interpret (BeamInterpretation Identity)) (tbl Uninterpreted)
      perField = Rep.cpure (Proxy @(FromBackendRowI be)) (Comp fromBackendRowI)

  gValuesNeeded pBackend _ _ =
       foldl' (+) 0 $ Rep.collapse perField
   where
      perField :: Rep (K Int) (tbl Uninterpreted)
      perField = Rep.cpure (Proxy @(FromBackendRowI be)) (valuesNeededI pBackend)

{-------------------------------------------------------------------------------
  Internal

  NOTE: the superclass constraints on the 'FromBackendRowI' instances match
  instance heads in beam, but his is what is used in the definition of
  'GFromBackendRow' itself, so we stick with it (and use @MonoLocalBinds@).
-------------------------------------------------------------------------------}

class FromBackendRowI be x where
  fromBackendRowI :: FromBackendRowM be (Interpret (BeamInterpretation Identity) x)
  valuesNeededI   :: Proxy be -> K Int x

instance FromBackendRow be x => FromBackendRowI be (Uninterpreted x) where
  fromBackendRowI = Interpret . unK1 <$> fromBeam
    where
      fromBeam :: FromBackendRowM be (K1 R x ())
      fromBeam = gFromBackendRow (Proxy @(K1 R (Exposed x)))

  valuesNeededI pBackend = K $
      gValuesNeeded
        pBackend
        (Proxy @(K1 R (Exposed x)))
        (Proxy @((K1 R x)))

instance FromBackendRow be (tbl Identity) => FromBackendRowI be (tbl Uninterpreted) where
  fromBackendRowI = Interpret . unK1 <$> fromBeam
    where
      fromBeam :: FromBackendRowM be (K1 R (tbl Identity) ())
      fromBeam = gFromBackendRow (Proxy @(K1 R (tbl Exposed)))

  valuesNeededI pBackend = K $
      gValuesNeeded
        pBackend
        (Proxy @(K1 R (tbl Exposed)))
        (Proxy @(K1 R (tbl Identity)))

instance FromBackendRow  be (tbl (Nullable Identity))
      => FromBackendRowI be (tbl (Nullable Uninterpreted)) where
  fromBackendRowI = Interpret . unK1 <$> fromBeam
    where
      fromBeam :: FromBackendRowM be (K1 R (tbl (Nullable Identity)) ())
      fromBeam = gFromBackendRow (Proxy @(K1 R (tbl (Nullable Exposed))))

  valuesNeededI pBackend = K $
      gValuesNeeded
        pBackend
        (Proxy @(K1 R (tbl (Nullable Exposed))))
        (Proxy @(K1 R (tbl (Nullable Identity))))
