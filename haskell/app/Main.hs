module Main where

import System.Environment
import System.Exit

import Data.GraphViz

helpStr :: String -> String
helpStr bin = "Usage: " ++ bin ++ " [input] [output] \n\
              \    Files should use the DOT language\n\
              \    https://graphviz.org/doc/info/lang.html"

parseArgs :: [String] -> IO ExitCode
parseArgs [input, output] = putStrLn "This should work" >> return ExitSuccess
parseArgs _ = getProgName >>= putStrLn . helpStr >> return (ExitFailure 1)

main :: IO ()
main = getArgs >>= parseArgs >>= exitWith
