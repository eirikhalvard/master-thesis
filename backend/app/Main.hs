module Main where

import Cli (executeParser)
import Program (runProgram)

main :: IO ()
main = runProgram =<< executeParser
