module Data.Record.Plugin.WithRDP (
    -- * Annotations
    WithoutRDP.LargeRecordOptions(..)
  , WithoutRDP.largeRecord
    -- * For use by ghc
  , plugin
  ) where

import Control.Monad

import GHC.Plugins (Plugin (..), defaultPlugin)

import qualified RecordDotPreprocessor as RDP
import qualified Data.Record.Plugin as WithoutRDP

plugin :: Plugin
plugin = combine WithoutRDP.plugin RDP.plugin

combine p q = defaultPlugin {
      parsedResultAction = \args summary ->
            parsedResultAction p args summary
        >=> parsedResultAction q args summary
    , pluginRecompile = \args ->
            (<>)
        <$> pluginRecompile p args
        <*> pluginRecompile q args
    }
