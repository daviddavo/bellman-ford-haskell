{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module BellmanFord where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import qualified Data.Graph.Inductive.Graph as G

import Infinite

type BFResultElem v = (Maybe G.Node, Infinite v)

-- Receives number of nodes and returns init distance and pred
initBF :: (MArray a (BFResultElem v) m, Real v) =>
    (G.Node, G.Node) -> 
    G.Node ->
    m (a G.Node (BFResultElem v))
initBF (a,b) s = do
    arr <- newArray (a,b) (Nothing, PosInf)
    writeArray arr s (Nothing, F 0)
    return arr

-- Relaxes a given edge
--   D[edge.to] = min(D[edge.to], D[edge.from] + edge.cost)
relaxEdge :: (MArray a (BFResultElem v) m, Real v) =>
    a G.Node (BFResultElem v) -> 
    G.LEdge v ->
    m ()
relaxEdge l (f, t, cost) = do
    (_, dfr) <- readArray l f
    (_, dto) <- readArray l t
    let aux = dfr + F cost
    when (aux < dto) (writeArray l t (Just f, aux))

-- Relaxes all edges in the graph
relaxAllEdges :: (MArray a (BFResultElem v) m, Real v, G.Graph g) => 
    a G.Node (BFResultElem v) -> 
    g l v -> 
    m ()
relaxAllEdges arr gr = mapM_ (relaxEdge arr) (G.labEdges gr)

-- for i = 1 to G.nodes - 1
--   for each edge (u,v)
--     relax(u,v)
bfMainLoop :: (MArray a (BFResultElem v) m, Real v, G.Graph g) =>
    G.Node ->                       -- Remaining nodes
    a G.Node (BFResultElem v) ->    -- Array of results
    g l v ->                        -- Graph
    m ()
bfMainLoop n arr gr = replicateM_ n $ relaxAllEdges arr gr

-- if (to.d > from.d + w(from,to))
bfCatchNode :: (MArray a (BFResultElem v) m, Real v) =>
    a G.Node (BFResultElem v) ->
    G.LEdge v ->
    m ()
bfCatchNode l (f, t, cost) = do
    (x, dfr) <- readArray l f
    (_, dto) <- readArray l t
    when (dto > dfr + F cost)  (writeArray l t (x, NegInf))

bfCatchNodes :: (MArray a (BFResultElem v) m, Real v, G.Graph g) =>
    a G.Node (BFResultElem v) ->
    g l v ->
    m ()
bfCatchNodes arr gr = mapM_ (bfCatchNode arr) (G.labEdges gr)

-- Executes bellmanFord on given array
bellmanFordA :: (MArray a (BFResultElem v) m, Real v, G.Graph g) =>
    g l v ->
    G.Node ->
    m (a G.Node (BFResultElem v))
bellmanFordA gr s = do
    let (i,f) = G.nodeRange gr
    arr <- initBF (i,f) s
    bfMainLoop f arr gr
    bfCatchNodes arr gr
    return arr

-- | Executes the Bellman-Ford algorithm using default monads and configurations
-- Receives the graph and the vertex to which to calculate distances
-- Returns the predecessor array and the costs
bellmanFord :: forall g v l. (G.Graph g, Real v) => g l v -> G.Node -> [(G.Node, BFResultElem v)]
bellmanFord gr s = runST $ (bellmanFordA gr s :: ST s (STArray s G.Node (BFResultElem v))) >>= getAssocs
