{-# LANGUAGE ScopedTypeVariables #-}

module Data.Record.Anonymous.TcPlugin.EquivClasses (
    constructEquivClasses
  , canonicalize
  ) where

import Data.Bifunctor
import Data.Foldable (toList)
import Data.Graph (Graph, Vertex)
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Graph as Graph
import qualified Data.Map   as Map
import qualified Data.Set   as Set

-- | Given a set of equivalent pairs, map every value to canonical value
--
-- Example with two classes:
--
-- >>> constructEquivClasses [(1, 2), (4, 5), (2, 3)]
-- fromList [(1,1),(2,1),(3,1),(4,4),(5,4)]
--
-- Adding one element that connects both classes:
--
-- >>> constructEquivClasses [(1, 2), (4, 5), (2, 3), (3, 4)]
-- fromList [(1,1),(2,1),(3,1),(4,1),(5,1)]
constructEquivClasses :: forall a. Ord a => [(a, a)] -> Map a a
constructEquivClasses equivs =
     Map.unions $ map (pickCanonical . map fromVertex . toList) $
       Graph.components graph
  where
    allValues :: Set a
    allValues = Set.fromList $ concatMap (\(x, y) -> [x, y]) equivs

    toVertex   :: a -> Vertex
    fromVertex :: Vertex -> a

    toVertex   a = Map.findWithDefault (error "toVertex: impossible")   a $
                     Map.fromList $ zip (Set.toList allValues) [1..]
    fromVertex v = Map.findWithDefault (error "fromVertex: impossible") v $
                     Map.fromList $ zip [1..] (Set.toList allValues)

    graph :: Graph
    graph = Graph.buildG (1, Set.size allValues) $
              map (bimap toVertex toVertex) equivs

    -- Given a previously established equivalence class, construct a mapping
    -- that maps each value to an (arbitrary) canonical value.
    pickCanonical :: [a] -> Map a a
    pickCanonical cls = Map.fromList $ zip cls (repeat (minimum cls))

canonicalize :: Ord a => Map a a -> a -> a
canonicalize canon x = Map.findWithDefault x x canon