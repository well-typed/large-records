{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- The point of this module is to verify that largeRecord does not generate
-- redundant constraints
{-# OPTIONS_GHC -Werror -Wredundant-constraints #-}

module Test.Record.Sanity.EqualFieldTypes (tests) where

import Data.Record.TH
import Test.Tasty

largeRecord defaultPureScript [d|
      data R a = MkR {
            field1 :: a
          , field2 :: a
          }
        deriving (Show, Eq)
    |]

-- The test is compilation itself
tests :: TestTree
tests = testGroup "Test.Record.Sanity.EqualFieldTypes" []
