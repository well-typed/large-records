{-# LANGUAGE ExistentialQuantification #-}

-- | Debugging support
module Data.Record.Anonymous.Internal.Debugging (
    ShowViaRecoverRTTI(..)
  ) where

import Debug.RecoverRTTI (anythingToString)

data ShowViaRecoverRTTI f = forall x. ShowViaRecoverRTTI (f x)

instance Show (ShowViaRecoverRTTI f) where
  show (ShowViaRecoverRTTI x) = anythingToString x
