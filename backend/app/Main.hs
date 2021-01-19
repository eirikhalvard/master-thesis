module Main where

import SerializeOutput

main :: IO ()
main = writeExampleToFile "../frontend/data/elm-input.json"
