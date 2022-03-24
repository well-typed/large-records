{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generic instance for HList using LargeRecords generics
module Experiment.Generics_LR (gtoJSON) where

import Data.Kind
import Data.SOP.Dict
import GHC.Exts (Any)
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import Data.Record.Generic
import Data.Record.Generic.JSON (gtoJSON)
import qualified Data.Record.Generic.Rep.Internal as Rep

import Bench.HList
import Infra.ShowType
import Infra.Tree

{-------------------------------------------------------------------------------
  Reflect type-level tree to the term level
-------------------------------------------------------------------------------}

class ReflectOne (f :: Type -> Type) (x :: Type) where
  reflectOne :: Proxy x -> f Any

class ReflectTree (f :: Type -> Type) (xs :: Tree Type) where
  reflectTree :: Proxy xs -> Tree (f Any)

type role ReflectTree nominal phantom

instance ReflectTree f 'Zero where
  reflectTree _ = Zero

instance ReflectOne f x => ReflectTree f ('One x) where
  reflectTree _ = One (reflectOne (Proxy @x))

instance ( ReflectOne f x1
         , ReflectOne f x2
         ) => ReflectTree f ('Two x1 x2) where
  reflectTree _ = Two (reflectOne (Proxy @x1)) (reflectOne (Proxy @x2))

instance ( ReflectOne  f x
         , ReflectTree f l
         , ReflectTree f r
         ) => ReflectTree f ('Branch x l r) where
  reflectTree _ =
      Branch
        (reflectOne  (Proxy @x))
        (reflectTree (Proxy @l))
        (reflectTree (Proxy @r))

{-------------------------------------------------------------------------------
  Constructing vector of dictionaries
-------------------------------------------------------------------------------}

instance c a => ReflectOne (Dict c) (a :: Type) where
  reflectOne _ = unsafeCoerce (Dict :: Dict c a)

{-------------------------------------------------------------------------------
  Construct metadata for each field
-------------------------------------------------------------------------------}

instance KnownSymbol (ShowType a) => ReflectOne FieldMetadata (a :: Type) where
  reflectOne _ = FieldMetadata (Proxy @(ShowType a)) FieldLazy

type family HListMetadata (xs :: [Type]) :: [(Symbol, Type)] where
  HListMetadata '[]       = '[]
  HListMetadata (x ': xs) = '(ShowType x, x) ': HListMetadata xs

{-------------------------------------------------------------------------------
  Putting the pieces together
-------------------------------------------------------------------------------}

hlistDict :: forall c (xs :: [Type]).
     ReflectTree (Dict c) (ToTree xs)
  => Proxy xs -> [Dict c Any]
hlistDict _ = treeToList $ reflectTree (Proxy @(ToTree xs))

class    ReflectTree (Dict c) (ToTree xs) => Constraints_HList xs c
instance ReflectTree (Dict c) (ToTree xs) => Constraints_HList xs c

instance ReflectTree FieldMetadata (ToTree xs) => Generic (HList xs) where
  type Constraints (HList xs) = Constraints_HList xs
  type MetadataOf  (HList xs) = HListMetadata xs

  dict _ = Rep.unsafeFromListAny $ hlistDict (Proxy @xs)
  from   = Rep.unsafeFromListAny . hlistToListAny
  to     = \xs -> case hlistFromListAny (Rep.toListAny xs) of
                    SomeHList xs' -> unsafeCoerce xs'

  metadata _ = Metadata {
        recordName          = "Record"
      , recordConstructor   = "MkRecord"
      , recordSize          = length fields
      , recordFieldMetadata = Rep.unsafeFromListAny fields
      }
    where
      fields :: [FieldMetadata Any]
      fields = treeToList $ reflectTree (Proxy @(ToTree xs))


