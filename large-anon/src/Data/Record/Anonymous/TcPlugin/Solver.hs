{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.TcPlugin.Solver (
    solve
  ) where

import Data.Bifunctor
import Data.Maybe (catMaybes)
import Data.Traversable (forM)

import Data.Record.Anonymous.TcPlugin.Constraints.AllFields
import Data.Record.Anonymous.TcPlugin.Constraints.HasField
import Data.Record.Anonymous.TcPlugin.Constraints.KnownFields
import Data.Record.Anonymous.TcPlugin.Constraints.KnownHash
import Data.Record.Anonymous.TcPlugin.Constraints.Project
import Data.Record.Anonymous.TcPlugin.GhcTcPluginAPI
import Data.Record.Anonymous.TcPlugin.NameResolution
import Data.Record.Anonymous.TcPlugin.Parsing
import Data.Record.Anonymous.TcPlugin.TyConSubst

{-------------------------------------------------------------------------------
  Top-level solver
-------------------------------------------------------------------------------}

solve :: ResolvedNames -> TcPluginSolver
solve rn given wanted =
--  trace _debugInput  $
--  trace _debugParsed $
    do (solved, new) <- fmap (bimap catMaybes concat . unzip) $ concatM [
           forM parsedAllFields   $ uncurry (solveAllFields   rn)
         , forM parsedHasField    $ uncurry (solveHasField    rn)
         , forM parsedKnownFields $ uncurry (solveKnownFields rn)
         , forM parsedKnownHash   $ uncurry (solveKnownHash   rn)
         , forM parsedProject     $ uncurry (solveProject     rn)
         ]
       return $ TcPluginOk solved new
  where
    tcs :: TyConSubst
    tcs = mkTyConSubst given

    parsedAllFields   :: [(Ct, GenLocated CtLoc CAllFields)]
    parsedHasField    :: [(Ct, GenLocated CtLoc CHasField)]
    parsedKnownFields :: [(Ct, GenLocated CtLoc CKnownFields)]
    parsedKnownHash   :: [(Ct, GenLocated CtLoc CKnownHash)]
    parsedProject     :: [(Ct, GenLocated CtLoc CProject)]

    parsedAllFields   = parseAll' (withOrig (parseAllFields   tcs rn)) wanted
    parsedHasField    = parseAll' (withOrig (parseHasField    tcs rn)) wanted
    parsedKnownFields = parseAll' (withOrig (parseKnownFields tcs rn)) wanted
    parsedKnownHash   = parseAll' (withOrig (parseKnownHash   tcs rn)) wanted
    parsedProject     = parseAll' (withOrig (parseProject     tcs rn)) wanted

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
        , concat ["parsedAllFields:   ", showSDocUnsafe $ ppr parsedAllFields]
        , concat ["parsedHasField:    ", showSDocUnsafe $ ppr parsedHasField]
        , concat ["parsedKnownFields: ", showSDocUnsafe $ ppr parsedKnownFields]
        , concat ["parsedKnownHash:   ", showSDocUnsafe $ ppr parsedKnownFields]
        , concat ["parsedProject:     ", showSDocUnsafe $ ppr parsedProject]
        , concat ["tcs (TyConSubst):  ", showSDocUnsafe $ ppr tcs]
        ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA
