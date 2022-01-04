{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -ddump-ds-preopt #-}

module Test.Record.Experiments.ToTreeAtUseSite where

import Data.Kind
import Data.Proxy

import Test.Record.Experiments.Util

class EmptyClass (xs :: Tree Type) where

instance EmptyClass 'Zero
instance EmptyClass ('One x)
instance EmptyClass ('Two x1 x2)
instance (EmptyClass l, EmptyClass r) => EmptyClass ('Branch x l r)

{-# NOINLINE requireEmptyClass #-}
requireEmptyClass :: EmptyClass xs => Proxy xs -> ()
requireEmptyClass _ = ()

abstracted :: forall xs. EmptyClass (ToTree xs) => Proxy xs -> ()
abstracted _ = requireEmptyClass (Proxy @(ToTree xs))

