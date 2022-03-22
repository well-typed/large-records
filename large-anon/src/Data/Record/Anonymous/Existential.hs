{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- TODO: Docs
-- TODO: Support for discovery of whether we can apply a recordLens
module Data.Record.Anonymous.Existential (
    -- * Recover record type information
    SomeField(..)
  , SomeFields(..)
  , someFields
  ) where

import Data.Bifunctor.Flip
import Data.Kind
import Data.Record.Generic
import Data.SOP
import GHC.Exts (Any)
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as Vector

import Data.Record.Anonymous.Internal.Row

{-------------------------------------------------------------------------------
  Auxiliary: satifying multiple constraints
-------------------------------------------------------------------------------}

type family Map (f :: k -> Constraint) (xs :: [k]) :: Constraint where
  Map f '[]       = ()
  Map f (x ': xs) = (f x, Map f xs)

reflectMap :: NP (Reflected :.: f) cs -> Reflected (Map f cs)
reflectMap Nil                    = Reflected
reflectMap (Comp Reflected :* rs) =
    case reflectMap rs of
      Reflected -> Reflected

{-------------------------------------------------------------------------------
  Recover record type information
-------------------------------------------------------------------------------}

-- | Some existentially qualified type (discovered at runtime) along with
-- evidence that it satisfies all required constraints
data SomeField (cs :: [k -> Constraint]) where
  SomeField :: NP (Flip Dict a) cs -> SomeField cs

reflectFieldDicts :: forall k (cs :: [k -> Constraint]) (r :: [(Symbol, k)]).
     SListI cs
  => Proxy r
  -> [(String, SomeField cs)]
  -> Reflected (Map (AllFields r) cs)
reflectFieldDicts _ fields = reflectMap $ hmap (Comp . aux) (indices @cs)
  where
    aux :: Index cs c -> Reflected (AllFields r c)
    aux i = reflectAllFields $ \_proxyR _proxyC ->
              Vector.fromList $ map (getDict i . snd) fields

    getDict :: Index cs c -> SomeField cs -> Dict c Any
    getDict i (SomeField dicts) = co $ runFlip (project i dicts)

    co :: Dict c a -> Dict c Any
    co = unsafeCoerce

someFieldMetadata :: String -> FieldMetadata Any
someFieldMetadata name =
    case (someSymbolVal name) of
      SomeSymbol p -> FieldMetadata p FieldStrict

data SomeFields (cs :: [k -> Constraint]) where
  SomeFields ::
       (KnownFields r, Map (AllFields r) cs)
    => Proxy r -> SomeFields cs

someFields :: forall k (cs :: [k -> Constraint]).
     SListI cs
  => [(String, SomeField cs)] -> SomeFields cs
someFields fields =
    withReflected $ reflectKnownFields $ \_proxyR ->
      map (someFieldMetadata . fst) fields
  where
    -- The reflected 'KnownFields' determines the shape of the record
    withReflected :: forall (r :: [(Symbol, k)]).
         Reflected (KnownFields r)
      -> SomeFields cs
    withReflected Reflected =
        case reflectFieldDicts p fields of
          Reflected -> SomeFields p
      where
        p = Proxy @r

{-------------------------------------------------------------------------------
  Internal auxiliary: SOP util
-------------------------------------------------------------------------------}

data Index :: [k] -> k -> Type where
  IZ :: Index (k ': ks) k
  IS :: Index ks k -> Index (l ': ks) k

indices :: forall ks. SListI ks => NP (Index ks) ks
indices =
    case sList :: SList ks of
      SNil  -> Nil
      SCons -> IZ :* hmap IS indices

project :: Index xs x -> NP f xs -> f x
project IZ     (x :* _)  = x
project (IS i) (_ :* xs) = project i xs
