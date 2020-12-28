{-# LANGUAGE FlexibleContexts #-}
module BellmanFord (BFResult, bellmanFord) where

import Data.Array.IO
import qualified Data.Graph.Inductive.Graph as G

data Infinite a = NegInf | Just a | PosInf
data BFResult a = R a | InInfCycle | ReachInfCycle

type BFResultElem v = (Maybe G.Node, Infinite v)

-- Receives number of nodes and returns init distance and pred
initBF :: (MArray a (BFResultElem v) m) => Int -> m (a G.Node (BFResultElem v))
initBF n = newArray (1::G.Node, n) (Nothing, NegInf)

-- Relaxes a given node (internal loop)
relax :: (MArray a (BFResultElem v) m) => G.Node -> m (a G.Node (BFResultElem v))

-- | Executes the Bellman-Ford algorithm
-- Receives the graph and the vertex to which to calculate distances
-- Returns the predecessor array and the costs
bellmanFord :: G.Graph g => g l v -> G.Node -> ([G.Node], [BFResult v])
bellmanFord _ _ = error "To Be Implemented"
