{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.Operators () where

import Data.Kind (Type)

import Data.Record.Plugin

-- Some type family (e.g. servant record-style API def).
type family tag :- route :: Type

{-# ANN type X largeRecord #-}
data X mode = MkX { _fx1 :: (mode :- Int) }

{-# ANN type Y largeRecord #-}
data Y mode = MkY { _fy1 :: mode :- Int }
