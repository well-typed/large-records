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
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE PolyKinds             #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.NamedWildCards where

{-# ANN type X largeRecord #-}
data X = MkX { _x :: Int }

{-# ANN type Y largeRecord #-}
data Y a = MkY { _y :: a }

{-# ANN type Z largeRecord #-}
data Z a (b :: a) = MkZ { _z :: a }

{-# ANN type Empty largeRecord #-}
data Empty = MkEmpty {}
