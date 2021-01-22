{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Generic.Size.After.R010 where

import Data.Record.Generic
import Data.Record.Generic.TH

import Test.Record.Generic.Size.Infra

largeRecord defaultOptions (recordOfSize 10)
