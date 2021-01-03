import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.Invariant

import Data.Array.MArray
import Data.Array.IO
import Data.Graph.Inductive.Graph hiding (pre)
import Data.Graph.Inductive.PatriciaTree
import Infinite
import BellmanFord

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
    ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [infiniteTests, bellmanFordTests]
