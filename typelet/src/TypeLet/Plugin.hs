module TypeLet.Plugin (plugin) where

import Prelude hiding (cycle)

import Data.Traversable (forM)

import GHC.Plugins (Plugin(..), defaultPlugin, purePlugin)

import TypeLet.Plugin.Constraints
import TypeLet.Plugin.GhcTcPluginAPI
import TypeLet.Plugin.NameResolution
import TypeLet.Plugin.Substitution

{-------------------------------------------------------------------------------
  Top-level plumbing
-------------------------------------------------------------------------------}

plugin :: Plugin
plugin = defaultPlugin {
      pluginRecompile  = purePlugin
    , tcPlugin         = \_cmdline -> Just . mkTcPlugin $ TcPlugin {
                               tcPluginInit    = resolveNames
                             , tcPluginSolve   = solve
                             , tcPluginRewrite = \_st -> emptyUFM
                             , tcPluginStop    = \_st -> return ()
                             }
    }

{-------------------------------------------------------------------------------
  Constraint resolution

  General approach: regard @Let@ constraints as defining a substitution, and
  then resolve @Equal@ constraints by /applying/ that substitution and
  simplifying to a derived equality constraint (derived instead of a new wanted
  constraint, because we don't actually need the evidence).
-------------------------------------------------------------------------------}

-- | Main interface to constraint resolution
--
-- NOTE: For now we are completely ignoring the derived constraints.
solve :: ResolvedNames -> TcPluginSolver
solve rn given wanted
  | null wanted = simplifyGivens rn given
  | otherwise   = simplifyWanteds rn given wanted

-- | Simplify givens
--
-- We (currently?) never simplify any givens, so we just two empty lists,
-- indicating that there no constraints were removed and none got added.
simplifyGivens ::
     ResolvedNames  -- ^ Result of name resolution (during init)
  -> [Ct]           -- ^ Given constraints
  -> TcPluginM 'Solve TcPluginSolveResult
simplifyGivens _st _given = return $ TcPluginOk [] []

-- | Simplify wanteds
--
-- This function provides the key functionality of the plugin.
--
-- We resolve 'Equal' constraints to /nominal/ equality constraints: we want
-- 'cast' to resolve @Let@ bindings, but not additionally work as 'coerce'.
simplifyWanteds ::
     ResolvedNames  -- ^ Result of name resolution (during init)
  -> [Ct]           -- ^ Given constraints
  -> [Ct]           -- ^ Wanted constraints
  -> TcPluginM 'Solve TcPluginSolveResult
simplifyWanteds rn given wanted = do
    case parseAll (parseLet rn) given of
      Left err ->
        errWith $ formatInvalidLet <$> err
      Right lets -> do
        case letsToSubst lets of
          Left cycle ->
            errWith $ formatLetCycle cycle
          Right subst -> do
            (solved, new) <- fmap unzip $
              forM (parseAll' (withOrig (parseEqual rn)) wanted) $
                uncurry (solveEqual subst)
            return $ TcPluginOk solved new
  where
    -- Work-around bug in ghc, making sure the location is set correctly
    newWanted' :: CtLoc -> PredType -> TcPluginM 'Solve CtEvidence
    newWanted' l w = setCtLocM l $ newWanted l w

    errWith ::
         GenLocated CtLoc TcPluginErrorMessage
      -> TcPluginM 'Solve TcPluginSolveResult
    errWith (L l err) = do
        errAsTyp <- mkTcPluginErrorTy err
        mkErr <$> newWanted' l errAsTyp
      where
        mkErr :: CtEvidence -> TcPluginSolveResult
        mkErr = TcPluginContradiction . (:[]) . mkNonCanonical

    -- Solve an Equal constraint by applying the substitution and turning it
    -- into a nominal equality constraint
    solveEqual ::
         Subst
      -> Ct                       -- Original Equal constraint
      -> GenLocated CtLoc CEqual  -- Parsed Equal constraint
      -> TcPluginM 'Solve ((EvTerm, Ct), Ct)
    solveEqual subst orig (L l parsed) = do
        ev <- newWanted' l $
                mkEqPredRole
                  Nominal
                  (substTy subst (equalLHS parsed))
                  (substTy subst (equalRHS parsed))
        return (
            (evidenceEqual rn parsed, orig)
          , mkNonCanonical ev
          )
