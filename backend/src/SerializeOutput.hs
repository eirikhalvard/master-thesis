{-# LANGUAGE FlexibleContexts #-}

module SerializeOutput where

import Conflict (conflictErrorMsg)
import Convertable
import Data.Bifunctor
import Examples.MergeConflictExample
import Examples.SoundExample ()
import Types

import Control.Lens ()
import Data.Aeson (encodeFile)

writeExampleToFile ::
  ( ConvertableInput evolutionPlan TreeUserEvolutionPlan
  , ConvertableFromResult evolutionPlan
  ) =>
  FilePath ->
  MergeInput evolutionPlan ->
  MergeOutput ->
  IO ()
writeExampleToFile filename mergeInput mergeOutput = do
  encodeFile filename $ createElmExample mergeInput mergeOutput

writeExamplesToFile ::
  ( ConvertableInput evolutionPlan TreeUserEvolutionPlan
  , ConvertableFromResult evolutionPlan
  ) =>
  FilePath ->
  [(MergeInput evolutionPlan, MergeOutput)] ->
  IO ()
writeExamplesToFile filename = do
  encodeFile filename . fmap (uncurry createElmExample)

createElmExamples ::
  ( ConvertableInput evolutionPlan TreeUserEvolutionPlan
  , ConvertableFromResult evolutionPlan
  ) =>
  [(MergeInput evolutionPlan, MergeOutput)] ->
  ElmDataExamples
createElmExamples = ElmDataExamples . fmap (uncurry createElmExample)

createElmExample ::
  ( ConvertableInput evolutionPlan TreeUserEvolutionPlan
  , ConvertableFromResult evolutionPlan
  ) =>
  MergeInput evolutionPlan ->
  MergeOutput ->
  ElmMergeExample
createElmExample (MergeInput name base v1 v2 maybeExpected) result =
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
