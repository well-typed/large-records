{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Common.EmptyClass_List (
    EmptyClass
  , requireEmptyClass
  ) where

import Data.Proxy
import Data.Kind

class EmptyClass (xs :: [Type]) where

instance EmptyClass '[]
instance EmptyClass xs => EmptyClass (x ': xs)

{-# NOINLINE requireEmptyClass #-}
requireEmptyClass :: EmptyClass xs => Proxy xs -> ()
requireEmptyClass _ = ()


