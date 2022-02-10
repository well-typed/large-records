{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin #-}

import Data.Functor.Identity
import Data.Record.Plugin (LargeRecordOptions (..))

{-# ANN type A LargeRecordStrict #-}
data A = A {a :: Int, b :: String}
  deriving stock (Show, Eq, Ord)

exampleA A {b, a} = A {a = 1, b = "2"}

{-# ANN type B LargeRecordStrict #-}
data B a = B {a :: a, b :: String}
  deriving stock (Show, Eq, Ord)

exampleB B {b, a} = B {a = b, b = "2"}

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

{-# ANN type C LargeRecordStrict #-}
data C f = C {a :: HKD f Int, b :: HKD f String}
  deriving stock (Show, Eq, Ord)

exampleC1 :: C Identity
exampleC1 = C {a = 1, b = "2"}

exampleC2 :: C Maybe
exampleC2 = C {a = Just 1, b = Just "2"}

_ = (vectorFromA, vectorFromB, vectorToC)

main = do
  print (exampleA (A 1 "2"))
  print (exampleB B {b = "3", a = "4"})
  print exampleC1
  print exampleC2
  print
    ( (A {a = 1, b = "2"}).a,
      (B {a = True, b = "2"}).a,
      exampleC1.b,
      exampleC2.b
    )
