{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module BellmanFord.Monad where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.IO
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Monad as G

import BellmanFord
import Infinite

relaxAllEdgesM :: (MArray a (BFResultElem v) m, Real v, G.GraphM m g) =>
    a G.Node (BFResultElem v) ->
    m (g l v) ->
    m ()
relaxAllEdgesM arr gr = G.labEdgesM gr >>= mapM_ (relaxEdge arr)

bfMainLoopM :: (MArray a (BFResultElem v) m, Real v, G.GraphM m g) =>
    G.Node ->
    a G.Node (BFResultElem v) ->
    m (g l v) ->
    m ()
bfMainLoopM n arr gr = replicateM_ n $ relaxAllEdgesM arr gr

bfCatchNodesM :: (MArray a (BFResultElem v) m, Real v, G.GraphM m g) =>
    a G.Node (BFResultElem v) ->
    m (g l v) ->
    m ()
bfCatchNodesM arr gr = G.labEdgesM gr >>= mapM_ (bfCatchNode arr)

bellmanFordAM :: (MArray a (BFResultElem v) m, Real v, G.GraphM m g) =>
    m (g l v) ->
    G.Node ->
    m (a G.Node (BFResultElem v))
bellmanFordAM gr s = do
    (i,f) <- G.nodeRangeM gr
    arr <- initBF (i,f) s
    bfMainLoopM (f-i) arr gr
    bfCatchNodesM arr gr
    return arr

bellmanFordIO :: forall g v l. (G.GraphM IO g, Real v) => IO (g l v) -> G.Node -> IO [(G.Node, BFResultElem v)]
bellmanFordIO gr s = (bellmanFordAM gr s :: IO (IOArray G.Node (BFResultElem v))) >>= getAssocs
