{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Solver (
    solve
  ) where

import Data.Bifunctor
import Data.Maybe (catMaybes)
import Data.Traversable (forM)

import Data.Record.Anonymous.Plugin.Constraints.HasField
import Data.Record.Anonymous.Plugin.Constraints.RecordConstraints
import Data.Record.Anonymous.Plugin.Constraints.RecordMetadata
import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing

{-------------------------------------------------------------------------------
  Top-level solver
-------------------------------------------------------------------------------}

solve :: ResolvedNames -> TcPluginSolver
solve rn given wanted =
--  trace _debugInput $
--  trace _debugParsed $
    do (solved, new) <- fmap (bimap catMaybes concat . unzip) $ concatM [
           forM parsedHasField          $ uncurry (solveHasField          rn)
         , forM parsedRecordConstraints $ uncurry (solveRecordConstraints rn)
         , forM parsedRecordMetadata    $ uncurry (solveRecordMetadata    rn)
         ]
       return $ TcPluginOk solved new
  where
    parsedHasField          :: [(Ct, GenLocated CtLoc CHasField)]
    parsedRecordConstraints :: [(Ct, GenLocated CtLoc CRecordConstraints)]
    parsedRecordMetadata    :: [(Ct, GenLocated CtLoc CRecordMetadata)]

    parsedHasField =
        parseAll' (withOrig (parseHasField rn)) wanted
    parsedRecordConstraints =
        parseAll' (withOrig (parseRecordConstraints rn)) wanted
    parsedRecordMetadata =
        parseAll' (withOrig (parseRecordMetadata rn)) wanted

    _debugInput :: String
    _debugInput = unlines [
          "*** input"
        , concat [
              "given:"
            , showSDocUnsafe (ppr given)
            ]
        , concat [
              "wanted: "
            , showSDocUnsafe (ppr wanted)
            ]
        ]

    _debugParsed :: String
    _debugParsed = unlines [
          "*** parsed"
        , concat [
              "parsedHasField: "
            , showSDocUnsafe (ppr parsedHasField)
            ]
        , concat [
              "parsedRecordConstraints: "
            , showSDocUnsafe (ppr parsedRecordConstraints)
            ]
        , concat [
              "parsedRecordMetadata: "
            , showSDocUnsafe (ppr parsedRecordMetadata)
            ]
        ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA
