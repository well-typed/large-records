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
  , reflectKnownFields
  , reflectAllFields
  , reflectProject
  ) where

import Data.Record.Anon.Plugin.Internal.Runtime

{-------------------------------------------------------------------------------
  Reflection
-------------------------------------------------------------------------------}

data Reflected c where
  Reflected :: c => Reflected c

newtype WithKnownFields r = WKF (KnownFields r => Reflected (KnownFields r))

reflectKnownFields :: DictKnownFields k r -> Reflected (KnownFields r)
reflectKnownFields f = noInlineUnsafeCo (WKF Reflected) f

newtype WithAllFields r c = WAF (AllFields r c => Reflected (AllFields r c))

reflectAllFields :: DictAllFields k r c -> Reflected (AllFields r c)
reflectAllFields f = noInlineUnsafeCo (WAF Reflected) f

newtype WithProject f r r' = WR (Project f r r' => Reflected (Project f r r'))

reflectProject :: DictProject k f r r' -> Reflected (Project f r r')
reflectProject = noInlineUnsafeCo (WR Reflected)
