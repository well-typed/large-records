module Data.Record.Anonymous.SrcPlugin.Options (
    -- * Options
    Options(..)
  , parseOpts
    -- * Mode
  , Mode(..)
  , parseMode
  ) where

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Options = Options {
      debug   :: Bool
    , typelet :: Bool
    }

defaultOptions :: Options
defaultOptions = Options {
      debug   = False
    , typelet = False -- TODO: Should typelet be the default?
    }

parseOpts :: [String] -> Options
parseOpts = ($ defaultOptions) . foldr (.) id . map aux
  where
    aux :: String -> Options -> Options
    aux "debug"   opts = opts { debug   = True }
    aux "typelet" opts = opts { typelet = True }
    aux opt       _    = error $ "invalid option: " ++ show opt

{-------------------------------------------------------------------------------
  Mode
-------------------------------------------------------------------------------}

data Mode = Simple | Advanced

parseMode :: String -> Maybe Mode
parseMode "ANON"   = Just Simple
parseMode "ANON_F" = Just Advanced
parseMode _        = Nothing
