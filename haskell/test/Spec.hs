import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Array.MArray
import Data.Array.IO
import Data.Graph.Inductive.Graph hiding (pre)
import Data.Graph.Inductive.PatriciaTree
import Infinite
import BellmanFord

type TestArray = IO (IOArray Node (BFResultElem Int))

prop_InitBounds :: Property
prop_InitBounds = monadicIO $ do
    n <- pick arbitrary
    pre (n > 0)
    x <- run (initBF (1,n) :: TestArray)
    b <- run $ getBounds x
    l <- run $ getElems x
    assert (b == (1::Node, n))
    assert (all (==(Nothing, PosInf)) l)

prop_RelaxEdges :: Property
prop_RelaxEdges = monadicIO $ do
    n <- pick (choose (2, 1000) :: Gen Int)
    from <- pick $ choose (1, n)
    to <- pick $ choose (1, n)
    pre (from /= to)
    arr <- run (initBF (1,n) :: TestArray)
    run $ writeArray arr from (Nothing, F 0)
    run (relaxEdge arr (from,to,n) )
    (p,d) <- run $ readArray arr to
    assert ( (p,d) == (Just from, F n) )

prop_RelaxAllEdges :: Property
prop_RelaxAllEdges = monadicIO $ do
    n <- pick (choose (5, 1000) :: Gen Int)
    from <- pick $ choose (1,n)
    to <- pick $ choose (1,n)
    pre (from /= to)
    arr <- run (initBF (1,n) :: TestArray)
    run $ writeArray arr from (Nothing, F 0)
    let gr = mkGraph [(from, "a"), (to, "b")] [(from, to, n)] ::Gr String Int
    run (relaxAllEdges arr gr)
    (p,d) <- run $ readArray arr to
    assert ( (p,d) == (Just from, F n) )

main :: IO ()
main = quickCheck prop_RelaxAllEdges
