module Main where

import Examples.MergeConflictExample (multipleAdd)
import Examples.SoundExample
import SerializeOutput
import ThreeWayMerge
import Types

mergeData :: [MergeInput]
mergeData =
  [ TreeUser soundExample
  , FlatModification multipleAdd
  ]

main :: IO ()
main = do
  writeElmExamplesToFile
    "../frontend/data/elm-input.json"
    [(soundExample, threeWayMerge soundExample)]
