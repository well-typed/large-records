module TypeLet.Plugin.Substitution (
    letsToSubst
  , Cycle(..)
  , formatLetCycle
  ) where

import Data.Bifunctor
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)

import qualified Data.Graph as G

import TypeLet.Plugin.Constraints
import TypeLet.Plugin.GhcTcPluginAPI

-- | Construct idempotent substitution
--
-- TODO: Not entirely sure if this is correct, might be too simplistic and/or an
-- abuse of the ghc API; it is a /whole/ lot simpler than @niFixTCvSubst@, which
-- is disconcerning. However, it seems to work for the examples so far; perhaps
-- our use case is simpler? Needs more thought.
letsToSubst ::
     [GenLocated CtLoc CLet]
  -> Either (Cycle (GenLocated CtLoc CLet)) TCvSubst
letsToSubst = fmap (uncurry zipTvSubst . unzip . go []) . inorder
  where
    go :: [(TyVar, Type)] -> [(TyVar, Type)] -> [(TyVar, Type)]
    go acc []         = acc
    go acc ((x, t):s) = go ((x, t) : map (second (subst1 x t)) acc) s

    subst1 :: TyVar -> Type -> Type -> Type
    subst1 x t = substTyWith [x] [t]

-- | Order the assignments
--
-- Suppose we have two assignments
--
-- > x := xT
-- > y := yT    where  x in (freevars yT)
--
-- Then the substitution should map @y@ to @(x := xT) yT@. We do this by
-- constructing the substitution in order, adding assignments one by one,
-- applying them to all assignments already in the accumulated substitution
-- as we go. In this example, this means adding @y := yT@ /first/, so that
-- we can apply @x := xT@ later (note that recursive definitions are
-- impossible in our use case).
--
-- In order to find the right order, we construct a graph of assignments. To
-- continue with our example, this graph will contain an edge
--
-- > (y := yT) -----> (x := xT)
--
-- The required assignment ordering is then obtained by topological sort.
inorder ::
     [GenLocated CtLoc CLet]
  -> Either (Cycle (GenLocated CtLoc CLet)) [(TyVar, Type)]
inorder lets =
    case cycles edges of
      c:_ -> Left c
      []  -> Right $ [
          (x, t)
        | (L _ (CLet _ x t), _, _) <- map nodeFromVertex $ G.topSort graph
        ]
  where
    graph          :: G.Graph
    nodeFromVertex :: G.Vertex -> (GenLocated CtLoc CLet, TyVar, [TyVar])
    _vertexFromKey :: TyVar -> Maybe G.Vertex
    (graph, nodeFromVertex, _vertexFromKey) = G.graphFromEdges edges

    edges :: [(GenLocated CtLoc CLet, TyVar, [TyVar])]
    edges = [
        ( l
        , y
        , [ x
          | L _ (CLet _ x _) <- lets
          , x `elemVarSet` (tyCoVarsOfType yT)
          ]
        )
      | l@(L _ (CLet _ y yT)) <- lets -- variables name match description above
      ]

-- | Format a cycle
--
-- We (arbitrarily) pick the first 'CLet' in the cycle for the location of the
-- error.
formatLetCycle ::
     Cycle (GenLocated CtLoc CLet)
  -> GenLocated CtLoc TcPluginErrorMessage
formatLetCycle (Cycle vs@(L l _ :| _)) = L l $
          Txt "Cycle in type-level let bindings: "
      :|: ( foldr1 (:|:)
          . intersperse (Txt ", ")
          . map (\(L _ l') -> formatCLet l')
          $ toList vs
          )

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Cycle in a graph
data Cycle a = Cycle (NonEmpty a)

cycles :: Ord key => [(node, key, [key])] -> [Cycle node]
cycles = mapMaybe aux . G.stronglyConnComp
  where
    aux :: G.SCC a -> Maybe (Cycle a)
    aux (G.AcyclicSCC _) = Nothing
    aux (G.CyclicSCC vs) =
        case vs of
          v:vs'      -> Just $ Cycle (v :| vs')
          _otherwise -> Nothing