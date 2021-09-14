{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

-- {-# OPTIONS_GHC -ddump-ds-preopt #-}
-- {-# OPTIONS_GHC -O #-}

module Test.Record.Experiments.Induction.List where

import Data.Proxy
import Data.Kind

class EmptyClass (xs :: [Type]) where

instance EmptyClass '[]
instance EmptyClass xs => EmptyClass (x ': xs)

{-# NOINLINE requireEmptyClass #-}
requireEmptyClass :: EmptyClass xs => Proxy xs -> ()
requireEmptyClass _ = ()


