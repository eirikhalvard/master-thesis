{-# LANGUAGE FlexibleContexts #-}

module SerializeOutput where

import Conflict (conflictErrorMsg)
import Convertable
import Data.Bifunctor
import Examples.SoundExample ()
import Types

import Control.Lens ()
import Data.Aeson (encodeFile)

writeElmExamplesToFile ::
  ( ConvertableInput evolutionPlan TreeUserEvolutionPlan
  , ConvertableFromResult evolutionPlan
  ) =>
  FilePath ->
  [(MergeInputData evolutionPlan, MergeOutput)] ->
  IO ()
writeElmExamplesToFile filename = do
  encodeFile filename . fmap (uncurry createElmExample)

createElmExamples ::
  ( ConvertableInput evolutionPlan TreeUserEvolutionPlan
  , ConvertableFromResult evolutionPlan
  ) =>
  [(MergeInputData evolutionPlan, MergeOutput)] ->
  ElmDataExamples
createElmExamples = ElmDataExamples . fmap (uncurry createElmExample)

createElmExample ::
  ( ConvertableInput evolutionPlan TreeUserEvolutionPlan
  , ConvertableFromResult evolutionPlan
  ) =>
  MergeInputData evolutionPlan ->
  MergeOutput ->
  ElmMergeExample
createElmExample (MergeInputData name base v1 v2 maybeExpected) result =
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
