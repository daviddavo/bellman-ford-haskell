import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck
import Test.Invariant
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Monad.ST

import Data.Array.MArray
import Data.Array.IO
import Data.Graph.Inductive.Graph hiding (pre)
import Data.Graph.Inductive.PatriciaTree
import Infinite
import BellmanFord

type TestArray = IO (IOArray Node (BFResultElem Int))

infiniteTests = testGroup "Infinite Type Tests" 
    [
        testProperty "F x + F y == F x+y" $ \x y -> F x + F y == F ((x + y)::Int),
        testProperty "commutative +" $ commutative $ \x y -> F (x::Int) + F y,
        testProperty "commutative *" $ commutative $ \x y -> F (x::Int) * F y,
        testProperty "signum" $ \x -> signum (F x::Infinite Int) == F (signum x),
        testProperty "abs" $ \x -> abs (F x::Infinite Int) == F (abs x),
        testProperty "show" $ \x -> show (F x::Infinite Int) == show x,
        testCase "abs -inf" $ abs NegInf @?= PosInf,
        testCase "abs +inf" $ abs PosInf @?= PosInf
    ]

bellmanFordTests = testGroup "Bellman Ford Tests" [bellmanFordFunctions]

bellmanFordFunctions = testGroup "Auxiliar Functions" 
    [
        testCase "initBF" $ do
            arr <- (initBF (1,100) :: TestArray) >>= getElems
            assertBool "All should be initialized" (all (==(Nothing, PosInf)) arr),
        testProperty "relaxEdge" $ monadicIO $ do
            n <- pick (choose (2, 1000) :: Gen Int)
            from <- pick $ choose (1, n)
            to <- pick $ choose (1, n)
            pre (from /= to)
            arr <- run (initBF (1,n) :: TestArray)
            run $ writeArray arr from (Nothing, F 0)
            run (relaxEdge arr (from,to,n) )
            (p,d) <- run $ readArray arr to
            assert ( (p,d) == (Just from, F n) ),
        testProperty "realaxAllEdges" $ monadicIO $ do
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
    ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [infiniteTests, bellmanFordTests]
