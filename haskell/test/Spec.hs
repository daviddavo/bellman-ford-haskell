import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Array.MArray
import Data.Array.IO
import qualified Data.Graph.Inductive.Graph as G
import BellmanFord

type TestArray = IO (IOArray G.Node (BFResultElem Integer))

prop_InitBounds :: Property
prop_InitBounds = monadicIO $ do
    n <- pick arbitrary
    pre (n > 0)
    --x <- (initBF n::TestArray)
    x <- run (initBF n :: TestArray)
    b <- run $ getBounds x
    l <- run $ getElems x
    assert (b == (1::G.Node, n))
    assert (all (==(Nothing, NegInf)) l)

main :: IO ()
main = quickCheck prop_InitBounds
