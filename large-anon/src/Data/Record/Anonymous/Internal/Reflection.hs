{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Reflection
--
-- Internal usage only.
module Data.Record.Anonymous.Internal.Reflection (
    Reflected(..)
  , unsafeReflectKnownFields
  , unsafeReflectAllFields
  , unsafeReflectProject
  ) where

import Data.Record.Anon.Plugin.Internal.Runtime

{-------------------------------------------------------------------------------
  Reflection
-------------------------------------------------------------------------------}

-- | Evidence of some constraint @c@
--
-- This is like 'Data.Record.Anon.Dict', but without a separate functor argument.
data Reflected c where
  Reflected :: c => Reflected c

newtype WithKnownFields r = WKF (KnownFields r => Reflected (KnownFields r))

unsafeReflectKnownFields :: DictKnownFields k r -> Reflected (KnownFields r)
unsafeReflectKnownFields f = noInlineUnsafeCo (WKF Reflected) f

newtype WithAllFields r c = WAF (AllFields r c => Reflected (AllFields r c))

unsafeReflectAllFields :: DictAllFields k r c -> Reflected (AllFields r c)
unsafeReflectAllFields f = noInlineUnsafeCo (WAF Reflected) f

newtype WithProject f r r' = WR (Project f r r' => Reflected (Project f r r'))

unsafeReflectProject :: DictProject k f r r' -> Reflected (Project f r r')
unsafeReflectProject = noInlineUnsafeCo (WR Reflected)
