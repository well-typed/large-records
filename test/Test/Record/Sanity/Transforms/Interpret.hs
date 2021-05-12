{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Record.Sanity.Transforms.Interpret (
    Uninterpreted
  , Interpreted
    -- * Newtype wrapper
  , Interpret(..)
  , liftInterpreted
  , liftInterpretedA2
  ) where

import Data.Kind

data Uninterpreted x

type family Interpreted (d :: dom) (f :: Type -> Type) (x :: Type)  :: Type

{-------------------------------------------------------------------------------
  Newtype wrapper
-------------------------------------------------------------------------------}

newtype Interpret d f x = Interpret (Interpreted d f x)

liftInterpreted ::
      (Interpreted d f x -> Interpreted d g y)
   -> (Interpret   d f x -> Interpret   d g y)
liftInterpreted f (Interpret x) = Interpret (f x)

liftInterpretedA2 ::
      Applicative m
   => (Interpreted d f x -> Interpreted d g y -> m (Interpreted d h z))
   -> (Interpret   d f x -> Interpret   d g y -> m (Interpret   d h z))
liftInterpretedA2 f (Interpret x) (Interpret y) = Interpret <$> f x y
