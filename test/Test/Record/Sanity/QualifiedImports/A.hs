{-# LANGUAGE TemplateHaskell #-}

module Test.Record.Sanity.QualifiedImports.A where

import Data.Record.TH

largeRecord defaultPureScript [d|
    data T a = MkT { x :: Int, y :: [a] }
  |]
