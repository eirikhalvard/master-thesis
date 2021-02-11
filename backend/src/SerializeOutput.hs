{-# LANGUAGE FlexibleContexts #-}

module SerializeOutput where

import Conflict (conflictErrorMsg)
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

writeToElm ::
  FilePath ->
  [ ( MergeInputData TreeUserEvolutionPlan
    , MergeResult TreeUserEvolutionPlan
    )
  ] ->
  IO ()
writeToElm filepath =
  encodeFile filepath . createElmExamples

createElmExamples ::
  [ ( MergeInputData TreeUserEvolutionPlan
    , MergeResult TreeUserEvolutionPlan
    )
  ] ->
  ElmDataExamples
createElmExamples = ElmDataExamples . fmap (uncurry createElmExample)

createElmExample ::
  MergeInputData TreeUserEvolutionPlan ->
  MergeResult TreeUserEvolutionPlan ->
  ElmMergeExample
createElmExample (MergeInputData name base v1 v2 maybeExpected) result =
  ElmMergeExample
    name
    ( [ ElmNamedEvolutionPlan "Base" $
          Right base
      , ElmNamedEvolutionPlan "Version 1" $
          Right v1
      , ElmNamedEvolutionPlan "Version 2" $
          Right v2
      ]
        ++ foldMap
          (pure . ElmNamedEvolutionPlan "Expected" . first conflictErrorMsg)
          maybeExpected
        ++ [ElmNamedEvolutionPlan "Actual" (first conflictErrorMsg result)]
    )
