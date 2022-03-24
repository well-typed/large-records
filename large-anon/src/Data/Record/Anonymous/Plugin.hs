module Data.Record.Anonymous.Plugin (plugin) where

import GHC.TcPlugin.API

import qualified GHC.Plugins

import Data.Record.Anonymous.TcPlugin.NameResolution
import Data.Record.Anonymous.TcPlugin.Rewriter
import Data.Record.Anonymous.TcPlugin.Solver
import Data.Record.Anonymous.SrcPlugin

-- | The large-anon plugins
--
-- This consists of two plugins:
--
-- 1. The type checker plugin forms the heart of this package. It solves
--    the various constraints we have on rows, and computes type-level metadata.
-- 2. The source plugin offers syntactic sugar for record construction, and
--    provides integration with the @typelet@ plugin.
plugin :: GHC.Plugins.Plugin
plugin = GHC.Plugins.defaultPlugin {
      GHC.Plugins.tcPlugin = \_args -> Just $
        mkTcPlugin tcPlugin
    , GHC.Plugins.parsedResultAction = \_args _modSummary ->
        sourcePlugin
    , GHC.Plugins.pluginRecompile =
        GHC.Plugins.purePlugin
    }

tcPlugin :: TcPlugin
tcPlugin = TcPlugin {
      tcPluginInit    = nameResolution
    , tcPluginSolve   = solve
    , tcPluginRewrite = rewrite
    , tcPluginStop    = const $ return ()
    }

