{-# LANGUAGE FlexibleContexts #-}

module SerializeOutput where

import Convertable
import Data.Bifunctor
import Examples.MergeConflictExample
import Examples.SoundExample
import ThreeWayMerge (conflictErrorMsg, threeWayMerge)
import Types

import Control.Lens ()
import Data.Aeson (encodeFile)

writeExampleToFile :: FilePath -> IO ()
writeExampleToFile filename = do
  let mergeInput@(MergeInputWithExpected input expected) = soundExample
      mergeOutput = threeWayMerge input
  print $ "Writing json to file " ++ filename
  encodeFile filename $ createElmExample mergeInput mergeOutput
  runFaultyTests

createElmExample ::
  (ConvertableInput evolutionPlan TreeUserEvolutionPlan) =>
  MergeInputWithExpected evolutionPlan ->
  MergeOutput ->
  ElmMergeExample
createElmExample (MergeInputWithExpected (MergeInput name base v1 v2) expected) result =
  ElmMergeExample
    name
    [ ElmNamedEvolutionPlan "Base" $
        Right (convertFrom base)
    , ElmNamedEvolutionPlan "Version 1" $
        Right (convertFrom v1)
    , ElmNamedEvolutionPlan "Version 2" $
        Right (convertFrom v2)
    , ElmNamedEvolutionPlan "Expected" $
        bimap conflictErrorMsg convertFrom expected
    , ElmNamedEvolutionPlan "Actual" $
        bimap conflictErrorMsg (uncurry convertFromMergeResult) result
    ]

runFaultyTests :: IO ()
runFaultyTests = do
  showExampleResult multipleAdd
