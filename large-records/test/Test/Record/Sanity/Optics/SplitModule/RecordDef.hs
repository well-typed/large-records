{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

-- | Record definitions for "Test.Record.Sanity.RDP.SplitModule"
--
-- See "Test.Record.Sanity.RDP.SplitModule" for details.
module Test.Record.Sanity.Optics.SplitModule.RecordDef (
    R1(..)
  , R2(..)
  , R3(..)
  , R4_WithLR(..)
  , R5_WithLR(..)
  ) where

{-# ANN type R1 largeRecord #-}
data R1 = MkR1 { r1_x :: Int, r1_y :: Bool }
  deriving (Show, Eq)

{-# ANN type R2 largeRecord #-}
data R2 = MkR2 { a :: Int, b :: Bool }
  deriving (Show, Eq)

{-# ANN type R3 largeRecord #-}
data R3 = MkR3 { a :: Int, b :: Char }
  deriving (Show, Eq)

{-# ANN type R4_WithLR largeRecord #-}
data R4_WithLR = MkR4_WithLR { r4_withLR_x :: Int, r4_withLR_y :: R5_WithLR }
  deriving (Show, Eq)

{-# ANN type R5_WithLR largeRecord #-}
data R5_WithLR = MkR5_WithLR { r5_withLR_x :: Char, r5_withLR_y :: Double }
  deriving (Show, Eq)
