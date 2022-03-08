{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.QualifiedImports.A (T(..)) where

{-# ANN type T largeRecordStrict #-}
data T a = MkT { x :: Int, y :: [a] }
