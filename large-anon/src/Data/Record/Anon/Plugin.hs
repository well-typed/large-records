{-# OPTIONS_HADDOCK hide #-}

-- | Type checker and source plugin for working with anonymous records
--
-- To use this, add
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}
--
-- to your module. You should not need to import this module; see
-- "Data.Record.Anon.Simple" or "Data.Record.Anon.Advanced" instead.
module Data.Record.Anon.Plugin (plugin) where

import Data.Record.Anon.Internal.Plugin (plugin)


