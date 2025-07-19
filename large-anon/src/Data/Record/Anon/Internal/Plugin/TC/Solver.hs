{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anon.Internal.Plugin.TC.Solver (
    solve
  ) where

import Data.Bifunctor
import Data.Maybe (catMaybes)
import Data.Traversable (forM)

import Data.Record.Anon.Internal.Plugin.TC.Constraints.AllFields
import Data.Record.Anon.Internal.Plugin.TC.Constraints.KnownFields
import Data.Record.Anon.Internal.Plugin.TC.Constraints.KnownHash
import Data.Record.Anon.Internal.Plugin.TC.Constraints.RowHasField
import Data.Record.Anon.Internal.Plugin.TC.Constraints.SubRow
import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Parsing

{-------------------------------------------------------------------------------
  Top-level solver
-------------------------------------------------------------------------------}

solve :: ResolvedNames -> TcPluginSolver
solve rn given wanted =
--  trace _debugInput  $
--  trace _debugParsed $
    do (solved, new) <- fmap (bimap catMaybes concat . unzip) $ concatM [
           forM parsedAllFields   $ uncurry (solveAllFields   rn)
         , forM parsedKnownFields $ uncurry (solveKnownFields rn)
         , forM parsedKnownHash   $ uncurry (solveKnownHash   rn)
         , forM parsedRowHasField $ uncurry (solveRowHasField rn)
         , forM parsedSubRow      $ uncurry (solveSubRow      rn)
         ]
       return $ TcPluginOk solved new
  where
    tcs :: TyConSubst
    tcs = mkTyConSubst given

    parsedAllFields   :: [(Ct, GenLocated CtLoc CAllFields)]
    parsedKnownFields :: [(Ct, GenLocated CtLoc CKnownFields)]
    parsedKnownHash   :: [(Ct, GenLocated CtLoc CKnownHash)]
    parsedRowHasField :: [(Ct, GenLocated CtLoc CRowHasField)]
    parsedSubRow      :: [(Ct, GenLocated CtLoc CSubRow)]

    parsedAllFields   = parseAll' (withOrig (parseAllFields   tcs rn)) wanted
    parsedKnownFields = parseAll' (withOrig (parseKnownFields tcs rn)) wanted
    parsedKnownHash   = parseAll' (withOrig (parseKnownHash   tcs rn)) wanted
    parsedRowHasField = parseAll' (withOrig (parseRowHasField tcs rn)) wanted
    parsedSubRow      = parseAll' (withOrig (parseSubRow      tcs rn)) wanted

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
        , concat ["parsedKnownFields: ", showSDocUnsafe $ ppr parsedKnownFields]
        , concat ["parsedKnownHash:   ", showSDocUnsafe $ ppr parsedKnownFields]
        , concat ["parsedRowHasField: ", showSDocUnsafe $ ppr parsedRowHasField]
        , concat ["parsedSubRow:      ", showSDocUnsafe $ ppr parsedSubRow]
        , concat ["tcs (TyConSubst):  ", showSDocUnsafe $ ppr tcs]
        ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA
