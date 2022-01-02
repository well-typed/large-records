{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Prop.Show.Large (
      FromRegular(..)
    , Example1(..)
    ) where

import Data.Record.TH

import qualified Test.Record.Prop.Show.Regular as Regular

class FromRegular a b | a -> b, b -> a where
  fromRegular :: a -> b

largeRecord defaultPureScript [d|
    data Example1 = MkExample1 {
          example1Field1 :: Int
        , example1Field2 :: Bool
        }
      deriving (Show)
  |]

instance FromRegular Regular.Example1 Example1 where
  fromRegular Regular.MkExample1{..} = MkExample1 {
        example1Field1 = example1Field1
      , example1Field2 = example1Field2
      }
