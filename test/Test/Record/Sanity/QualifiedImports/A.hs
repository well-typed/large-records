{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- The explicit export of MkT here is intentional: it tests for regressions
-- of issue #31.
module Test.Record.Sanity.QualifiedImports.A (T(MkT)) where

import Data.Record.TH

largeRecord defaultPureScript [d|
    data T a = MkT { x :: Int, y :: [a] }
  |]
