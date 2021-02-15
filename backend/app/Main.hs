module Main where

import Cli (executeParser)
import Program (runProgram, writeSomeExamples)

main :: IO ()
main = runProgram =<< executeParser

-- main :: IO ()
-- main = writeSomeExamples
