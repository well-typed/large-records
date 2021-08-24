{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Record.Sanity.QualifiedImports.B (T(..)) where

import Data.Record.TH

import qualified Test.Record.Sanity.QualifiedImports.A as A

largeRecord defaultPureScript [d|
    data T a = MkT { x :: Char, y :: A.T a }
  |]
