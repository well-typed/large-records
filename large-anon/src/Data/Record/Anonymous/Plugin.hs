module Data.Record.Anonymous.Plugin (plugin) where

import GHC.TcPlugin.API

import qualified GHC.Plugins

import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Solver

plugin :: GHC.Plugins.Plugin
plugin = GHC.Plugins.defaultPlugin {
      GHC.Plugins.tcPlugin = \_args -> Just $ mkTcPlugin tcPlugin
    , GHC.Plugins.pluginRecompile = GHC.Plugins.purePlugin
    }

tcPlugin :: TcPlugin
tcPlugin = TcPlugin {
      tcPluginInit    = nameResolution
    , tcPluginSolve   = solve
    , tcPluginRewrite = const $ emptyUFM
    , tcPluginStop    = const $ return ()
    }

