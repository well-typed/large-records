module Data.Record.Anon.Internal.Plugin (plugin) where

import GHC.TcPlugin.API

import qualified GHC.Plugins

import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Rewriter
import Data.Record.Anon.Internal.Plugin.TC.Solver
import Data.Record.Anon.Internal.Plugin.Source

-- | The large-anon plugins
--
-- This consists of two plugins:
--
-- 1. The type checker plugin forms the heart of this package. It solves
--    the various constraints we have on rows, and computes type-level metadata.
-- 2. The source plugin offers syntactic sugar for record construction.
plugin :: GHC.Plugins.Plugin
plugin = GHC.Plugins.defaultPlugin {
      GHC.Plugins.tcPlugin = \_args -> Just $
        mkTcPlugin tcPlugin
    , GHC.Plugins.parsedResultAction = \args _modSummary ->
        sourcePlugin args
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

