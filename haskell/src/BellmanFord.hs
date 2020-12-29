{-# LANGUAGE FlexibleContexts #-}
module BellmanFord where

import Control.Monad
import Data.Array.IO
import qualified Data.Graph.Inductive.Graph as G

import Infinite

data BFResult a = R a | InInfCycle | ReachInfCycle

type BFResultElem v = (Maybe G.Node, Infinite v)

-- Receives number of nodes and returns init distance and pred
initBF :: (MArray a (BFResultElem v) m) => Int -> m (a G.Node (BFResultElem v))
initBF n = newArray (1::G.Node, n) (Nothing, PosInf)

-- for edge in graph.edges
--   D[edge.to] = min(D[edge.to], D[edge.from] + edge.cost)

-- Relaxes a given edge (internal loop)
relaxEdge :: (MArray a (BFResultElem v) m, Num v, Ord v) => a G.Node (BFResultElem v) -> G.LEdge v -> m ()
relaxEdge l (f, t, cost) = do
    (_, dfr) <- readArray l f
    (_, dto) <- readArray l t
    let aux = dfr + F cost
    when (aux < dto) (writeArray l t (Just f, aux))

-- | Executes the Bellman-Ford algorithm
-- Receives the graph and the vertex to which to calculate distances
-- Returns the predecessor array and the costs
bellmanFord :: G.Graph g => g l v -> G.Node -> ([G.Node], [BFResult v])
bellmanFord _ _ = error "To Be Implemented"
