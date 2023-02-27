{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Record.Plugin.WithRDP (
    -- * Annotations
    WithoutRDP.LargeRecordOptions(..)
  , WithoutRDP.largeRecord
    -- * For use by ghc
  , plugin
  ) where

import Control.Monad

import qualified RecordDotPreprocessor as RDP

import Data.Record.Internal.GHC.Shim

import qualified Data.Record.Plugin as WithoutRDP

plugin :: Plugin
plugin = WithoutRDP.plugin <> RDP.plugin

-- | Limited 'Semigroup' instance that defines just enough to combine
-- @large-records@ and @record-dot-preprocessor@
instance Semigroup Plugin where
  p <> q = defaultPlugin {
      parsedResultAction = \args summary ->
            parsedResultAction p args summary
        >=> parsedResultAction q args summary
    , pluginRecompile = \args ->
            (<>)
        <$> pluginRecompile p args
        <*> pluginRecompile q args
    }
