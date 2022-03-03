{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generic instance for HList using SOP generics
module Experiment.Generics_SOP (gtoJSON) where

import Data.Aeson
import Data.Kind
import Data.SOP
import Generics.SOP (Generic(..), HasDatatypeInfo(..))
import Generics.SOP.Type.Metadata

import qualified Generics.SOP.JSON     as SOP
import qualified Generics.SOP.Metadata as SOP

import Infra.HList
import Infra.ShowType

gtoJSON :: (HasDatatypeInfo a, All2 ToJSON (Code a)) => a -> Value
gtoJSON = SOP.gtoJSON SOP.defaultJsonOptions

{-------------------------------------------------------------------------------
  Computing metadata
-------------------------------------------------------------------------------}

type family ComputeDatatypeInfo (xs :: [Type]) :: DatatypeInfo where
  ComputeDatatypeInfo xs =
    'ADT "SomeModule" "Record"
      '[ 'Record "MkRecord" (ComputeFieldInfo xs) ]
      '[ ComputeStrictnessInfo xs ]

type family ComputeStrictnessInfo (xs :: [Type]) :: [StrictnessInfo] where
  ComputeStrictnessInfo '[]       = '[]
  ComputeStrictnessInfo (_ ': xs) = DefaultStrictnessInfo
                                 ': ComputeStrictnessInfo xs

type DefaultStrictnessInfo =
  'StrictnessInfo
    'SOP.NoSourceUnpackedness
    'SOP.NoSourceStrictness
    'SOP.DecidedLazy

type family ComputeFieldInfo (xs :: [Type]) :: [FieldInfo] where
  ComputeFieldInfo '[]       = '[]
  ComputeFieldInfo (x ': xs) = 'FieldInfo (ShowType x) ': ComputeFieldInfo xs

{-------------------------------------------------------------------------------
  Generic instance proper
-------------------------------------------------------------------------------}

instance SListI xs => Generic (HList xs) where
  type Code (HList xs) = '[xs]

  from = SOP . Z . hlistToNP
  to   = hlistFromNP . unZ . unSOP

instance ( DemoteFieldInfos      (ComputeFieldInfo      xs) xs
         , DemoteStrictnessInfos (ComputeStrictnessInfo xs) xs
         ) => HasDatatypeInfo (HList xs) where
  type DatatypeInfoOf (HList xs) = ComputeDatatypeInfo xs
  datatypeInfo _ = demoteDatatypeInfo (Proxy @(ComputeDatatypeInfo xs))

