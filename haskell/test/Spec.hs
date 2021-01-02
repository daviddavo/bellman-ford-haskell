import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.Invariant

import Data.Array.MArray
import Data.Array.IO
import Data.Graph.Inductive.Graph hiding (pre)
import Data.Graph.Inductive.PatriciaTree
import Infinite
import BellmanFord

infinitePropertyTests = testGroup "Infinite Property Tests" 
    [
        SC.testProperty "F x + F y == F x+y" $ \x y -> F x + F y == F ((x + y)::Int),
        SC.testProperty "commutative +" $ commutative $ \x y -> F (x::Int) + F y
    ]

unitTests = testGroup "Unit tests" 
    [ testCase "Sample test" $ 1 @?= 2

    ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [infinitePropertyTests, unitTests]
