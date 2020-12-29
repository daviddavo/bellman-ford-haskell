import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Array.MArray
import Data.Array.IO
import qualified Data.Graph.Inductive.Graph as G
import Infinite
import BellmanFord

type TestArray = IO (IOArray G.Node (BFResultElem Int))

prop_InitBounds :: Property
prop_InitBounds = monadicIO $ do
    n <- pick arbitrary
    pre (n > 0)
    --x <- (initBF n::TestArray)
    x <- run (initBF n :: TestArray)
    b <- run $ getBounds x
    l <- run $ getElems x
    assert (b == (1::G.Node, n))
    assert (all (==(Nothing, PosInf)) l)

prop_RelaxEdges :: Property
prop_RelaxEdges = monadicIO $ do
    n <- pick (choose (2, 1000) :: Gen Int)
    from <- pick $ choose (1, n)
    to <- pick $ choose (1, n)
    pre (from /= to)
    arr <- run (initBF n :: TestArray)
    run $ writeArray arr from (Nothing, F 0)
    run (relaxEdge arr (from,to,n) )
    (p,d) <- run $ readArray arr to
    assert ( (p,d) == (Just from, F n) )

main :: IO ()
main = quickCheck prop_RelaxEdges
