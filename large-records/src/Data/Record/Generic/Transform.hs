{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- The 'HasNormalForm' constraint on 'normalize' and 'denormalize' is
-- redundant as far as ghc is concerned (it's just 'unsafeCoerce' after all),
-- but essential for type safety of these two functions.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Record.Generic.Transform (
    -- * Interpretation function
    Interpreted
  , Interpret(..)
    -- ** Working with the 'Interpreted' newtype wrapper
  , liftInterpreted
  , liftInterpretedA2
    -- * Normal form
    -- ** Existence
  , HasNormalForm
  , InterpretTo
  , IfEqual
    -- ** Construction
  , normalize
  , denormalize
    -- ** Specialized forms for the common case of a single type argument
  , Uninterpreted
  , DefaultInterpretation
  , normalize1
  , denormalize1
    -- ** Generalization of the default interpretation
  , StandardInterpretation(..)
  , toStandardInterpretation
  , fromStandardInterpretation
  ) where

import Data.Coerce
import Data.Kind
import Data.Proxy
import Data.SOP.BasicFunctors
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import Data.Record.Generic

{-------------------------------------------------------------------------------
  Interpretation function
-------------------------------------------------------------------------------}

type family Interpreted (d :: dom) (x :: Type) :: Type

newtype Interpret d x = Interpret (Interpreted d x)

{-------------------------------------------------------------------------------
  Working with the 'Interpreted' newtype wrapper
-------------------------------------------------------------------------------}

liftInterpreted ::
      (Interpreted dx x -> Interpreted dy y)
   -> (Interpret   dx x -> Interpret   dy y)
liftInterpreted f (Interpret x) = Interpret (f x)

liftInterpretedA2 ::
      Applicative m
   => (Interpreted dx x -> Interpreted dy y -> m (Interpreted dz z))
   -> (Interpret   dx x -> Interpret   dy y -> m (Interpret   dz z))
liftInterpretedA2 f (Interpret x) (Interpret y) = Interpret <$> f x y

{-------------------------------------------------------------------------------
  Normal forms
-------------------------------------------------------------------------------}

type HasNormalForm d x y = InterpretTo d (MetadataOf x) (MetadataOf y)

type family InterpretTo d xs ys :: Constraint where
  InterpretTo _ '[]             '[]             = ()
  InterpretTo d ('(f, x) ': xs) ('(f, y) ': ys) = IfEqual x (Interpreted d y)
                                                    (InterpretTo d xs ys)

type family IfEqual x y (r :: k) :: k where
  IfEqual actual   actual k = k
  IfEqual expected actual k = TypeError (
          'Text "Expected "
    ':<>: 'ShowType expected
    ':<>: 'Text " but got "
    ':<>: 'ShowType actual
    )

-- | Construct normal form
--
-- TODO: Documentation.
normalize ::
     HasNormalForm d x y
  => Proxy d
  -> Proxy y
  -> Rep I x -> Rep (Interpret d) y
normalize _ _ = unsafeCoerce

denormalize ::
     HasNormalForm d x y
  => Proxy d
  -> Proxy y
  -> Rep (Interpret d) y -> Rep I x
denormalize _ _ = unsafeCoerce

{-------------------------------------------------------------------------------
  Specialized forms for the common case of a single type argument

  The test ("Test.Record.Sanity.Transform") shows an example with two arguments.
-------------------------------------------------------------------------------}

data Uninterpreted x

data DefaultInterpretation (f :: Type -> Type)

type instance Interpreted (DefaultInterpretation f) (Uninterpreted x) = f x

normalize1 :: forall d f x.
     HasNormalForm (d f) (x f) (x Uninterpreted)
  => Proxy d
  -> Rep I (x f) -> Rep (Interpret (d f)) (x Uninterpreted)
normalize1 _ = normalize (Proxy @(d f)) (Proxy @(x Uninterpreted))

denormalize1 :: forall d f x.
     HasNormalForm (d f) (x f) (x Uninterpreted)
  => Proxy d
  -> Rep (Interpret (d f)) (x Uninterpreted) -> Rep I (x f)
denormalize1 _ = denormalize (Proxy @(d f)) (Proxy @(x Uninterpreted))

{-------------------------------------------------------------------------------
  Generalization of the default interpretation
-------------------------------------------------------------------------------}

class StandardInterpretation d f where
  standardInterpretation ::
       Proxy d
    -> ( Interpreted (d f) (Uninterpreted x) -> f x
       , f x -> Interpreted (d f) (Uninterpreted x)
       )

  default standardInterpretation ::
       Coercible (Interpreted (d f) (Uninterpreted x)) (f x)
    => Proxy d
    -> ( Interpreted (d f) (Uninterpreted x) -> f x
       , f x -> Interpreted (d f) (Uninterpreted x)
       )
  standardInterpretation _ = (coerce, coerce)

instance StandardInterpretation DefaultInterpretation f

toStandardInterpretation :: forall d f x.
     StandardInterpretation d f
  => Proxy d
  -> f x -> Interpret (d f) (Uninterpreted x)
toStandardInterpretation d fx = Interpret $
    snd (standardInterpretation d) fx

fromStandardInterpretation :: forall d f x.
     StandardInterpretation d f
  => Proxy d
  -> Interpret (d f) (Uninterpreted x) -> f x
fromStandardInterpretation d (Interpret fx) =
    fst (standardInterpretation d) fx
