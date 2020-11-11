module SerializeOutput where

import Example
import Types

import Data.Aeson

writeExampleToFile :: FilePath -> IO ()
writeExampleToFile filename = do
  print $ "Writing json to file " ++ filename
  encodeFile filename mergeResult
  where
    mergeResult =
      MergeResult
        [ MergeEvolutionPlan "base" baseEvolutionPlan
        , MergeEvolutionPlan "v1" v1EvolutionPlan
        , MergeEvolutionPlan "v2" v2EvolutionPlan
        , MergeEvolutionPlan "expected" expectedEvolutionPlan
        ]
