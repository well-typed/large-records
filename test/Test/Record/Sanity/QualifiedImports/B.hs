{-# LANGUAGE TemplateHaskell #-}

module Test.Record.Sanity.QualifiedImports.B where

import Data.Record.TH

import qualified Test.Record.Sanity.QualifiedImports.A as A

largeRecord defaultPureScript [d|
    data T a = MkT { x :: Char, y :: A.T a }
  |]
