{-# LANGUAGE FlexibleContexts #-}

module SerializeOutput where

import Conflict (conflictErrorMsg)
import Convertable
import Examples.SoundExample ()
import qualified Lenses as L
import Types

import Control.Lens
import Data.Aeson
import Data.Bifunctor
import Text.Pretty.Simple

------------------------------------------------------------------------
--                       Merge Result Printing                        --
------------------------------------------------------------------------

printResult ::
  (Eq evolutionPlan, Show evolutionPlan) =>
  MergeInputData evolutionPlan ->
  MergeResult evolutionPlan ->
  IO ()
printResult mergeInput mergeResult = do
  print $ "EXAMPLE " ++ mergeInput ^. L.name
  case mergeInput ^. L.maybeExpected of
    Nothing -> do
      print $ "NO EXPECTED OUTPUT GIVEN"
    Just expected -> do
      if expected == mergeResult
        then do
          print "THE RESULT WERE AS EXPECTED"
        else do
          print "THE RESULT WERE NOT AS EXPECTED"
          print "EXPECTED RESULT:"
          pPrint expected
  print "ACTUAL RESULT:"
  case mergeResult of
    Left err -> pPrint err
    Right model -> pPrint model

------------------------------------------------------------------------
--                   Write Successful Merge To File                   --
------------------------------------------------------------------------

writeResultToFile ::
  ToJSON evolutionPlan => FilePath -> evolutionPlan -> IO ()
writeResultToFile = encodeFile

------------------------------------------------------------------------
--                  Elm Frontend Data Serialization                   --
------------------------------------------------------------------------

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
