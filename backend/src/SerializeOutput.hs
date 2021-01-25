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
  let (MergeInputWithExpected input expected) = soundExample
      mergeOutput = threeWayMerge input
  encodeFile filename $ createElmExample input (Just expected) mergeOutput

createElmExamples ::
  (ConvertableInput evolutionPlan TreeUserEvolutionPlan) =>
  [(MergeInput evolutionPlan, Maybe (MergeResult evolutionPlan), MergeOutput)] ->
  ElmDataExamples
createElmExamples = ElmDataExamples . fmap (\(input, mExpected, output) -> createElmExample input mExpected output)

createElmExample ::
  (ConvertableInput evolutionPlan TreeUserEvolutionPlan) =>
  MergeInput evolutionPlan ->
  Maybe (MergeResult evolutionPlan) ->
  MergeOutput ->
  ElmMergeExample
createElmExample (MergeInput name base v1 v2) maybeExpected result =
  ElmMergeExample
    name
    ( [ ElmNamedEvolutionPlan "Base" $
          Right (convertFrom base)
      , ElmNamedEvolutionPlan "Version 1" $
          Right (convertFrom v1)
      , ElmNamedEvolutionPlan "Version 2" $
          Right (convertFrom v2)
      ]
        ++ foldMap
          (pure . ElmNamedEvolutionPlan "Expected" . bimap conflictErrorMsg convertFrom)
          maybeExpected
        ++ [ ElmNamedEvolutionPlan "Actual" $
              bimap conflictErrorMsg (uncurry convertFromMergeResult) result
           ]
    )
runFaultyTests :: IO ()
runFaultyTests = do
  showExampleResult multipleAdd
