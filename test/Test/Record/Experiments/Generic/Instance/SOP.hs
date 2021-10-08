{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generic instance for HList using SOP generics
module Test.Record.Experiments.Generic.Instance.SOP () where

import Data.Kind
import Data.SOP
import Generics.SOP (Generic(..), HasDatatypeInfo(..))
import Generics.SOP.Type.Metadata

import qualified Generics.SOP.Metadata as SOP

import Test.Record.Experiments.HList
import Test.Record.Experiments.Util

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

