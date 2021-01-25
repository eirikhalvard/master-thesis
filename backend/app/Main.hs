module Main where

import Examples.SoundExample
import SerializeOutput
import ThreeWayMerge

main :: IO ()
main =
  writeExampleToFile
    "../frontend/data/elm-input.json"
    soundExample
    (threeWayMerge soundExample)
