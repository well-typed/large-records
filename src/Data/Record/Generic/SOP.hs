{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Interop with @generics-sop@ generics
module Data.Record.Generic.SOP (
    -- | Translate between SOP representation and large-records representation
    Field(..)
  , fromSOP
  , toSOP
    -- | Translate constraints
  , toDictAll
    -- | Additional SOP functions
  , glowerBound
  ) where

import Data.Kind
import Data.Proxy
import Data.SOP.Dict (all_NP)
import Generics.SOP (SOP(..), NS(..), NP(..), SListI, All, Code, Compose)
import GHC.Exts (Any)
import GHC.TypeLits (Symbol)

import qualified Data.Vector  as V
import qualified Generics.SOP as SOP

import Data.Record.Generic
import Data.Record.Generic.LowerBound hiding (glowerBound)
import Data.Record.TH.Runtime (noInlineUnsafeCo)

{-------------------------------------------------------------------------------
  Conversion back and forth to generics-sop records

  NOTE: We do /not/ require @SListI (MetadataOf a)@ by default, as this would
  result in quadratic blow-up again. This is only required in this module for
  SOP interop.

  NOTE: We don't currently use @records-sop@, despite it being a /near/ perfect
  fit. The problem is that @records-sop@ is not generalized over a functor,
  which would make these functions less general than we need them to be.
-------------------------------------------------------------------------------}

newtype Field (f :: Type -> Type) (field :: (Symbol, Type)) where
  Field :: f (FieldType field) -> Field f field

deriving instance Show (f x) => Show (Field f '(nm, x))
deriving instance Eq   (f x) => Eq   (Field f '(nm, x))

fromSOP :: SListI (MetadataOf a) => NP (Field f) (MetadataOf a) -> Rep f a
fromSOP =
    Rep . V.fromList . SOP.hcollapse . SOP.hmap conv
  where
    conv :: Field f field -> K (f Any) field
    conv (Field fx) = K $ noInlineUnsafeCo fx

toSOP :: SListI (MetadataOf a) => Rep f a -> Maybe (NP (Field f) (MetadataOf a))
toSOP (Rep v) =
    SOP.hmap conv <$> SOP.fromList (V.toList v)
  where
    conv :: K (f Any) field -> Field f field
    conv (K fx) = Field (noInlineUnsafeCo fx)

{-------------------------------------------------------------------------------
  Translate constraints
-------------------------------------------------------------------------------}

-- | Translate constraints
--
-- When using 'toSOP', if you start with something of type
--
-- > Rep f a
--
-- you end up with something of type
--
-- > NP (Field f) (MetadataOf a)
--
-- When doing so, 'toDictAll' can translate
--
-- > Constraints a (Compose c f)
--
-- (which is useful over the original representation) to
--
-- > All (Compose c (Field f)) (MetadataOf a)
--
-- which is useful for the translated representation.
toDictAll ::
     forall f a c.
     ( Generic a
     , Constraints a (Compose c f)
     , All IsField (MetadataOf a)
     , forall nm x. c (f x) => c (Field f '(nm, x))
     )
  => Proxy f
  -> Proxy a
  -> Proxy c
  -> Dict (All (Compose c (Field f))) (MetadataOf a)
toDictAll _ _ _ =
    case toSOP dictT of
      Nothing -> error "toDictAll: invalid dictionary"
      Just d  -> all_NP (SOP.hcmap (Proxy @IsField) conv d)
  where
    dictT :: Rep (Dict (Compose c f)) a
    dictT = dict (Proxy @(Compose c f))

    conv :: IsField field
         => Field (Dict (Compose c f)) field
         -> Dict (Compose c (Field f)) field
    conv (Field Dict) = Dict

{-------------------------------------------------------------------------------
  Additional SOP generic functions
-------------------------------------------------------------------------------}

glowerBound :: (SOP.Generic a, All LowerBound xs, Code a ~ '[xs]) => a
glowerBound = SOP.to . SOP . Z $ SOP.hcpure (Proxy @LowerBound) (I lowerBound)
