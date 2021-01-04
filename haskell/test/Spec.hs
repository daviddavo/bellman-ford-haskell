import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck
import Test.Invariant
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Array.MArray
import Data.Array.IO
import Data.Graph.Inductive.Graph hiding (pre)
import qualified Data.Graph.Inductive.Example as Ex
import Data.Graph.Inductive.PatriciaTree
import Infinite
import BellmanFord

type TestArray = IO (IOArray Node (BFResultElem Int))

clr3_646 :: Gr Char Int
clr3_646 = mkGraph (zip [1..] ['s', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'u']) [
    (1,2,3), (1,4,5), (1,6,2), (2,3,-4), (4,5,6), (5,4,-3), (6,7,3), (7,6,-6), (3,8,4), (5,8,8), (7,8,7)]

clr3_652 :: Gr Char Int
clr3_652 = mkGraph (zip [1..] ['s', 't', 'x', 'y', 'z']) [
    (1,2,6), (2,3,5), (3,2,-2), (1,4,7), (2,4,8), (4,3,-3), (2,5,-4), (5,1,2), (4,5,9), (5,3,7)]
clr3_659 = Ex.clr528

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

bellmanFordTests = testGroup "Bellman Ford Tests" [bellmanFordFunctions, bellmanFordIntegrity]

bellmanFordFunctions = testGroup "Auxiliar Functions" 
    [
        testCase "initBF" $ do
            arr <- (initBF (1,100) 1 :: TestArray) >>= getElems
            head arr @?= (Nothing, F 0)
            mapM_ (@?=(Nothing, PosInf)) (tail arr),
        testProperty "relaxEdge" $ monadicIO $ do
            n <- pick (choose (2, 1000) :: Gen Int)
            from <- pick $ choose (1, n)
            to <- pick $ choose (1, n)
            pre (from /= to)
            arr <- run (initBF (1,n) from :: TestArray)
            run (relaxEdge arr (from,to,n) )
            (p,d) <- run $ readArray arr to
            assert ( (p,d) == (Just from, F n) ),
        testProperty "relaxAllEdges single" $ monadicIO $ do
            n <- pick (choose (5, 1000) :: Gen Int)
            from <- pick $ choose (1,n)
            to <- pick $ choose (1,n)
            pre (from /= to)
            arr <- run (initBF (1,n) from :: TestArray)
            let gr = mkGraph [(from, "a"), (to, "b")] [(from, to, n)] ::Gr String Int
            run (relaxAllEdges arr gr)
            (p,d) <- run $ readArray arr to
            assert ( (p,d) == (Just from, F n) ),
        -- It's dificult to test relaxAllEdges because we could have different
        -- implementations as long that when we execute it v-1 times we have
        -- a correct result array, it depends on the graph implementation
        -- Sample Graphs from Cormen, Leiserson, Rivest (3rd edition)
        testCase "bfMainLoop clr659" $ do
            arr <- (initBF (1,5) 1 :: TestArray)
            bfMainLoop 5 arr clr3_659
            l <- getAssocs arr
            mapM_ (uncurry (@?=)) (zip l $ zip [1..] [
                (Nothing, F 0), (Just 4, F 8), (Just 2, F 9), (Just 1, F 5), (Just 4, F 7)]),
        testCase "bfMainLoop clr652" $ do
            arr <- (initBF (1,5) 1 :: TestArray)
            bfMainLoop 5 arr clr3_652
            l <- getAssocs arr
            mapM_ (uncurry (@?=)) (zip l $ zip [1..] [
                (Nothing, F 0), (Just 3, F 2), (Just 4, F 4), (Just 1, F 7), (Just 2, F (-2))]),
        testCase "bellManFordA clr646" $ do
            l <- (bellmanFordA clr3_646 1::TestArray) >>= getAssocs
            mapM_ (uncurry (@?=)) (zip l $ zip [1..] [
                (Nothing, F 0), (Just 1, F 3), (Just 2, -F 1), (Just 1, F 5),
                (Just 4, F 11), (Just 7, NegInf), (Just 7, NegInf), (Just 7, NegInf), (Nothing, PosInf)])
    ]

bellmanFordIntegrity = testGroup "Integrity Test" [
        testCase "bellmanFord clr646" $ do
            let l = bellmanFord clr3_646 1
            mapM_ (uncurry (@?=)) (zip l $ zip [1..] [
                (Nothing, F 0), (Just 1, F 3), (Just 2, -F 1), (Just 1, F 5),
                (Just 4, F 11), (Just 7, NegInf), (Just 7, NegInf), (Just 7, NegInf), (Nothing, PosInf)])
    ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [infiniteTests, bellmanFordTests]
