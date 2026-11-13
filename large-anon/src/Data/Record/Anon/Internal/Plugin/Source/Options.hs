module Data.Record.Anon.Internal.Plugin.Source.Options (
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
      debug   :: Bool -- ^ Dump generated code
    , noapply :: Bool -- ^ Omit the call to 'applyDiff'
    }

defaultOptions :: Options
defaultOptions = Options {
      debug   = False
    , noapply = False
    }

parseOpts :: [String] -> Options
parseOpts = ($ defaultOptions) . foldr (.) id . map aux
  where
    aux :: String -> Options -> Options
    aux "debug"   opts = opts { debug   = True }
    aux "noapply" opts = opts { noapply = True }
    aux opt       _    = error $ "invalid option: " ++ show opt

{-------------------------------------------------------------------------------
  Mode
-------------------------------------------------------------------------------}

data Mode = Simple | Advanced

parseMode :: String -> Maybe Mode
parseMode "ANON"   = Just Simple
parseMode "ANON_F" = Just Advanced
parseMode _        = Nothing
